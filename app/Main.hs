{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Linear (V4(..))

import Foreign.C.Types
import Display
import Figure


loop :: Display -> [SDL.Event] ->  IO ()
loop display (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return ()
        _ -> do
            draw display
            SDL.present (renderer display)
            loop display es

loop display [] = do
    events <- SDL.pollEvents
    loop display events

initView :: IO Display
initView = do
    win <- SDL.createWindow "Geometric constructor" $ SDL.defaultWindow {
        SDL.windowMode = SDL.FullscreenDesktop
    }
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    let draw = do {
        SDL.rendererDrawColor renderer $= V4 255 255 255 255;
        SDL.clear renderer
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
    display <- initView
    loop display []
    Font.free $ font display
    Font.quit
    SDL.quit
