{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
module Construction (
    Construction(..),
    Choice(..),
    Orientation(..),

    equilateralTriangle,
    copySegment
) where

import Control.Monad
import Linear (V2(..))


data Orientation = Left | Right


class Monad constr => Construction constr p l c | constr -> p l c where
    line :: p -> p -> constr l
    circle :: p -> p -> constr c
    intersectCircles :: c -> c -> constr [p]
    intersectLineCircle :: l -> c -> constr [p]

    displayCircle :: c -> constr ()
    displayLine :: p -> p -> constr ()

class Choice e s where
    choose :: e -> e -> s -> e


equilateralTriangle :: (Choice p Orientation, Construction constr p l c) =>
        Bool -> p -> p -> Orientation -> constr p
equilateralTriangle detailed a b orientation = do
        c1 <- circle a b
        c2 <- circle b a
        intersections <- intersectCircles c1 c2
        when detailed $ do
            displayCircle c1
            displayCircle c2
        case intersections of
            [x, y] -> let c = choose x y orientation in do
                    displayLine a c
                    displayLine b c
                    return c

            _ -> fail "The IMPOSSIBLE happened!"

copySegment :: (Choice p Orientation, Construction constr p l c) =>
        Bool -> p -> p -> p -> Orientation -> constr p
copySegment detailed a b c orientation = do
        d <- equilateralTriangle False a b orientation
        da <- line d a
        db <- line d b
        c_bc <- circle b c
        es <- intersectLineCircle db c_bc
        when detailed $ do
            displayLine a b
            displayLine a d
            displayLine b d
            sequence_ $ map (displayLine b) es
            displayCircle c_bc
        return d
