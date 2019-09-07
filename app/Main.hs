{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Data.Text (pack)
import Linear (V2(..), V4(..))
import Control.Monad.State
import Foreign.C.Types
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
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
    intersectLineCircle c l = return $ Fig.intersectLineCircle c l

    displayLine a b = modDisplay (\d -> d {
            draw = draw d >> (display d $ Fig.Segment a b)
        })
    displayCircle c = modDisplay (\d -> d{
            draw = draw d >> display d c
        })

showCopySegment :: DisplayState ()
showCopySegment = do
        let a = Fig.Point (V2 2 5) (Just $ Fig.Label "A" (V2 (-5) 0))
        let b = Fig.Point (V2 4 3) (Just $ Fig.Label "B" (V2 5 0))
        let c = Fig.Point (V2 5 3) (Just $ Fig.Label "C" (V2 5 0))
        Constr.displayLine b c
        d <- Constr.copySegment True a b c Constr.Left
        modDisplay (\disp -> disp { draw = draw disp >> (display disp $ d {
            Fig.ptLabel = Just $ Fig.Label "D" (V2 0 (-20))
        })})
        return ()

pointOfStr :: String -> Maybe String -> Fig.Point
pointOfStr s label =
        let (x, y) = read s :: (Double, Double) in
        Fig.Point (V2 x y) $ fmap (flip Fig.Label (V2 0 0) . pack) label

selectConstruction :: [String] -> IO (DisplayState ())
selectConstruction ["equilateralTriangle", astr, bstr] = do
        let a = pointOfStr astr $ Just "A"
        let b = pointOfStr bstr $ Just "B"
        return $ do
            Constr.displayLine a b
            c <- Constr.equilateralTriangle True a b Constr.Left
            modDisplay (\disp -> disp {
                draw = draw disp >> (display disp $ c {
                    Fig.ptLabel = Just (Fig.Label "C" (V2 0 0))
                })
            })

selectConstruction ["copySegment", astr, bstr, cstr] = do
        let a = pointOfStr astr $ Just "A"
        let b = pointOfStr astr $ Just "B"
        let c = pointOfStr astr $ Just "C"
        return $ do
            Constr.displayLine b c
            d <- Constr.copySegment True a b c Constr.Left
            modDisplay (\disp -> disp {
                draw = draw disp >> (display disp $ d {
                    Fig.ptLabel = Just (Fig.Label "D" (V2 0 0))
                })
            })

selectConstruction _ = do
        hPutStrLn stderr "Unrecognized construction! Initializing empty view."
        return $ return ()

present :: DisplayState ()
present = do
        display <- getDisplay
        liftIO $ SDL.clear (renderer display)
        liftIO $ draw display
        liftIO $ SDL.present (renderer display)

loop :: DisplayState () -> [SDL.Event] -> DisplayState ()
loop constr [] = SDL.pollEvents >>= loop constr
loop constr (e : es) = case SDL.eventPayload e of
        SDL.KeyboardEvent ke -> handleKeyPress ke
        SDL.QuitEvent -> return ()
        _ -> present >> loop constr es
    where
    handleKeyPress :: SDL.KeyboardEventData -> DisplayState ()
    handleKeyPress (SDL.KeyboardEventData _win _mot _rep keysym) =
            case SDL.unwrapKeycode $ SDL.keysymKeycode keysym of
                13 -> constr >> present >> loop constr es
                27 -> return () -- ESC
                _ -> loop constr es

initView :: IO Display
initView = do
    win <- SDL.createWindow "Geometric constructor" $ SDL.defaultWindow {
        SDL.windowMode = SDL.FullscreenDesktop,
        SDL.windowResizable = True
    }
    renderer <- SDL.createRenderer win 2 SDL.defaultRenderer
    let draw = do {
        SDL.rendererDrawColor renderer $= V4 255 255 255 255;
        SDL.clear renderer;
        SDL.rendererDrawColor renderer $= V4 0 0 0 255;
    }

    font <- Font.load "/usr/share/fonts/ubuntu/UbuntuMono-B.ttf" 20
    return $ Display {
        window = win,
        renderer = renderer,
        font = font,
        draw = draw
    }


main :: IO ()
main = do
    SDL.initialize [ SDL.InitEvents, SDL.InitVideo ]
    Font.initialize
    constr <- getArgs >>= selectConstruction
    display <- initView >>= runDisplay (loop constr [])
    Font.free $ font display
    Font.quit
    SDL.quit
