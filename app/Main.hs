{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Linear (V2(..), V4(..))

import Control.Monad.State
import Foreign.C.Types
import Display
import Vector (angle)
import qualified Figure as Fig
import qualified Construction as Constr


instance Constr.Choice Fig.Point Constr.Orientation where
    choose a b o =
            let alpha = angle $ Fig.loc a
                beta = angle $ Fig.loc b
            in
            if condition o alpha beta then a else b
        where
        condition Constr.Left = (<=)
        condition Constr.Right = (>)

instance Constr.Construction DisplayState Fig.Point Fig.Line Fig.Circle where
    line a b = return $ Fig.Line a (Fig.loc a - Fig.loc b)
    circle a b = return $ Fig.Circle a (Fig.distance a b)
    intersectCircles c1 c2 = return $ Fig.intersectCircles c1 c2

    displayLine a b = modDisplay (\d -> d {
            draw = draw d >> (display d $ Fig.Segment a b)
        })
    displayCircle c = modDisplay (\d -> d{
            draw = draw d >> display d c
        })

present :: DisplayState ()
present = do
        display <- getDisplay
        liftIO $ SDL.clear (renderer display)
        liftIO $ draw display
        liftIO $ SDL.present (renderer display)

showEquilateralTriangle :: DisplayState ()
showEquilateralTriangle = do
        let a = Fig.Point (V2 4 4) Nothing
        let b = Fig.Point (V2 6 7) Nothing
        modDisplay (\d -> d { draw = draw d >> (display d $ Fig.Segment a b) })
        _ <- Constr.equilateralTriangle True a b Constr.Left
        return ()

loop :: [SDL.Event] -> DisplayState ()
loop [] = SDL.pollEvents >>= loop
loop (e : es) = case SDL.eventPayload e of
        SDL.KeyboardEvent ke -> handleKeyPress ke
        SDL.QuitEvent -> return ()
        _ -> present >> loop es
    where
    handleKeyPress :: SDL.KeyboardEventData -> DisplayState ()
    handleKeyPress (SDL.KeyboardEventData _win _mot _rep keysym) =
            case SDL.unwrapKeycode $ SDL.keysymKeycode keysym of
                13 -> showEquilateralTriangle >> present >> loop es
                27 -> return () -- ESC
                _ -> loop es

initView :: IO Display
initView = do
    win <- SDL.createWindow "Geometric constructor" $ SDL.defaultWindow {
        SDL.windowMode = SDL.FullscreenDesktop
    }
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    let draw = do {
        SDL.rendererDrawColor renderer $= V4 255 255 255 255;
        SDL.clear renderer;
        SDL.rendererDrawColor renderer $= V4 0 0 0 255;
    }

    font <- Font.load "/usr/share/fonts/ubuntu/UbuntuMono-B.ttf" 1
    return $ Display {
        window = win,
        renderer = renderer,
        font = font,
        draw = draw
    }


main :: IO ()
main = do
    SDL.initialize [ SDL.InitEvents ]
    Font.initialize
    display <- initView >>= runDisplay (loop [])
    Font.free $ font display
    Font.quit
    SDL.quit
