module Figure (
    Label(..),
    Point(..),
    Line(..),
    Circle(..),
    Segment(..),
    distance,
    extend,
    translate,
    intersectLines,
    intersectCircles,
    intersectLineCircle
) where

import Data.Text (Text, unpack)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Linear (V2(..))
import Linear.Matrix ((!*), det22, inv22)

import Math (quadratic)
import Vector (unit, scale, magnitude, fromPolar, angle)

data Label = Label {
    text :: Text,
    position :: V2 Int32 -- in pixels
}

data Point = Point {
    loc :: V2 Double,
    ptLabel :: Maybe Label
}

data Line = Line {
    anchor :: Point,
    dir :: V2 Double
}

data Segment = Segment Point Point

data Circle = Circle {
    center :: Point,
    radius :: Double
}

extend :: Segment -> Line
extend (Segment a b) = Line { anchor = a, dir = unit (loc a - loc b)}

distance :: Point -> Point -> Double
distance a b = magnitude (loc b - loc a)


instance Show Point where
    show (Point (V2 x y) label) =
        fromMaybe "Point" (fmap (unpack . text) label) ++
         " = (" ++ show x ++ ", " ++ show y ++ ")"

class Translatable t where
    translate :: V2 Double -> t -> t


instance Translatable Point where
    translate v p = Point { loc = loc p + v, ptLabel = Nothing }

instance Translatable Circle where
    translate v c = c { center = translate v $ center c }

instance Translatable Segment where
    translate v (Segment a b) = Segment (translate v a) (translate v b)


-- Nothing if lines are parallel
intersectLines :: Line -> Line -> Maybe Point
intersectLines l1 l2 =
        let a = loc (anchor l2) - loc (anchor l1)
            V2 x1 y1 = dir l1
            V2 x2 y2 = dir l2
            m = V2 (V2 x1 (-x2)) (V2 y1 (-y2))
        in
        if det22 m == 0 then Nothing else
            let V2 sc _ = inv22 m !* a in
            Just (translate (scale sc $ dir l1) $ anchor l1)

intersectCircles :: Circle -> Circle -> [Point]
intersectCircles (Circle c1 r1) (Circle c2 r2) =
        let d@(V2 x y) = loc c2 - loc c1
            dist = magnitude d
            cosAlpha = (dist ** 2 + r1 ** 2 - r2 ** 2) / (2 * r1 * dist)
            vs = case compare cosAlpha 1 of
                GT -> []
                EQ -> [ fromPolar r1 $ angle d ]
                LT -> [ fromPolar r1 theta | theta <- [
                        angle d + acos cosAlpha,
                        angle d - acos cosAlpha
                    ]]
            trans = if (x < 0) || (x == 0 && y < 0)
                then flip translate c1 . scale (-1)
                else flip translate c1
        in
        map trans vs

intersectLineCircle :: Line ->  Circle -> [Point]
intersectLineCircle (Line a (V2 0 y)) (Circle c r) =
        let V2 xz yz = loc c - loc a
            results = quadratic (1, 0, xz ** 2 - r ** 2)
        in
        map (flip translate c . V2 (-xz)) results
intersectLineCircle (Line a (V2 x 0)) (Circle c r) =
        let V2 xz yz = loc c - loc a
            results = quadratic (1, 0, yz ** 2 - r ** 2)
        in
        map (flip translate c . flip V2 (-yz)) results
intersectLineCircle (Line al (V2 xv yv)) (Circle cc r) =
        let V2 xz yz = loc cc - loc al
            t = (yv * xz - xv * yz) / xv
            a = (yv ** 2) / (xv ** 2) + 1
            b = 2 * (yv ** 2) / xv
            c = t ** 2 - r ** 2
        in do
            x <- quadratic (a, b ,c)
            v <- return $ V2 x (x * yv / xv)
            return $ translate v cc
