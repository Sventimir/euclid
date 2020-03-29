{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
module Construction (
    Construction(..),
    Choice(..),
    Orientation(..),

    equilateralTriangle,
    copySegment,
    perpendicularThrough
) where

import Control.Monad
import Data.Maybe


data Orientation = Left | Right


class Monad constr => Construction constr p l c | constr -> p l c where
    line :: p -> p -> constr l
    circle :: p -> p -> constr c
    intersectLines :: l -> l -> constr (Maybe p)
    intersectCircles :: c -> c -> constr [p]
    intersectLineCircle :: l -> c -> constr [p]

    closer :: p -> p -> p -> constr p
    farther :: p -> p -> p -> constr p

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
        e <- case es of
            [x] -> return x
            [x, y] -> farther d x y
            _ -> fail "The IMPOSSIBLE happened!"
        c_de <- circle d e
        fs <- intersectLineCircle da c_de
        f <- case fs of
            [x] -> return x
            [x, y] -> closer a x y
            _ -> fail "The IMPOSSIBLE happened"
        when detailed $ do
            displayLine a b
            displayLine a d
            displayLine b d
            displayLine b e
            displayCircle c_bc
            displayCircle c_de
        displayLine a f
        return f

perpendicularThrough :: (Choice p Orientation, Construction constr p l c) =>
        Bool -> p -> p -> p -> constr (Maybe p)
perpendicularThrough detailed a b t = do
        l <- line a b
        cir <- circle t a
        inters <- intersectLineCircle l cir
        case inters of
            [c, d] -> do
                e <- equilateralTriangle detailed c d Construction.Left
                when detailed $ do
                    displayCircle cir
                    displayLine t e
                return $ Just e
