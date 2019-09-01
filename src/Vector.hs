module Vector (
    scale,
    magnitude,
    unit,
    fromPolar,
    angle
) where

import Linear (V2(..))


scale :: Num a => a -> V2 a -> V2 a
scale f (V2 x y) = V2 (f * x) (f * y)

magnitude :: V2 Double -> Double
magnitude (V2 x y) = sqrt (x ** 2 + y ** 2)

unit :: V2 Double -> V2 Double
unit v = scale (recip $ magnitude v) v

fromPolar :: Floating a => a -> a -> V2 a
fromPolar r theta = V2 (r * cos theta) (r * sin theta)

angle :: Floating a => V2 a -> a
angle (V2 x y) = atan (y / x)
