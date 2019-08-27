module Main where

import qualified SDL.Init as SDL

main :: IO ()
main = do
    SDL.initialize [ SDL.InitEvents ]
    SDL.quit
