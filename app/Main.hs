{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL (($=))
import qualified SDL
import Linear (V4(..))


loop :: SDL.Renderer -> [SDL.Event] ->  IO ()
loop renderer (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return ()
        _ -> SDL.present renderer >> loop renderer es
loop renderer [] = do
    events <- SDL.pollEvents
    loop renderer events

initView :: IO (SDL.Window, SDL.Renderer)
initView = do
    win <- SDL.createWindow "Geometric constructor" $ SDL.defaultWindow {
        SDL.windowMode = SDL.FullscreenDesktop
    }
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.clear renderer
    SDL.present renderer
    return (win, renderer)


main :: IO ()
main = do
    SDL.initialize [ SDL.InitEvents ]
    (_, renderer) <- initView
    loop renderer []
    SDL.quit
