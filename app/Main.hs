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
    intersectLines l1 l2 = return $ Fig.intersectLines l1 l2
    intersectCircles c1 c2 = return $ Fig.intersectCircles c1 c2
    intersectLineCircle l c = return $ Fig.intersectLineCircle l c

    closer p a b = return $ if Fig.distance p a <= Fig.distance p b then a else b
    farther p a b = return $ if Fig.distance p a >= Fig.distance p b then a else b

    displayLine a b = modDisplay (\d -> d {
            draw = draw d >> (display d $ Fig.Segment a b)
        })
    displayCircle c = modDisplay (\d -> d{
            draw = draw d >> display d c
        })

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
        let b = pointOfStr bstr $ Just "B"
        let c = pointOfStr cstr $ Just "C"
        return $ do
            Constr.displayLine b c
            d <- Constr.copySegment True a b c Constr.Left
            modDisplay (\disp -> disp {
                draw = draw disp >> (display disp $ d {
                    Fig.ptLabel = Just (Fig.Label "D" (V2 0 0))
                })
            })

selectConstruction ["doubleAngle", length, turn] = do
        let a = Fig.Point (V2 (5 + read length) 5) $ Just (Fig.Label (pack "A") $ V2 0 0)
        let b = Fig.Point (V2 (cos readTurn) (sin readTurn)) $ Just (Fig.Label (pack "B") $ V2 0 0)
        let o = Fig.Point (V2 5 5) Nothing
        return $ do
            Constr.displayLine o a
            Constr.displayLine o b
            unitCirc <- Constr.circle o a
            Constr.displayCircle unitCirc
            maybe_c <- Constr.perpendicularThrough False o b a
            case maybe_c of
                Just c -> do
                    l <- Constr.line a c
                    inters <- Constr.intersectLineCircle l unitCirc
                    case inters of
                        [x, y] ->
                            modDisplay (\d -> d {
                                draw = draw d >> (display d $ Fig.Segment x y)
                            })


    where
    readTurn = 5 + 2 * pi * read turn * read length

selectConstruction ["polars", x, y, bx, by, cx, cy] = do
    let o = Fig.Point (V2 5 5) Nothing
    let circ = Fig.Circle o 2
    let a = Fig.Point (V2 (read x) (read y)) (Just $ Fig.Label (pack "a") $ V2 0 0)
    let abd = Fig.Line a (V2 (read bx) (read by))
    let ace = Fig.Line a (V2 (read cx) (read cy))
    return $ do
        [b, d] <- Constr.intersectLineCircle abd circ
        [c, e] <- Constr.intersectLineCircle ace circ
        Just f <- Constr.intersectLines (Fig.extend $ Fig.Segment b e) (Fig.extend $ Fig.Segment c d)
        Just g <- Constr.intersectLines (Fig.extend $ Fig.Segment b c) (Fig.extend $ Fig.Segment d e)
        [x, y] <- Constr.intersectLineCircle (Fig.extend $ Fig.Segment f g) circ
        Constr.displayCircle circ
        Constr.displayLine a b
        Constr.displayLine b d
        Constr.displayLine a c
        Constr.displayLine c e
        Constr.displayLine b c
        Constr.displayLine b e
        Constr.displayLine c d
        Constr.displayLine d e
        Constr.displayLine f g
        Constr.displayLine b g
        Constr.displayLine d g
        Constr.displayLine x y
        modDisplay (\dis -> dis {
            draw = do
                draw dis
                display dis b
                display dis c
                display dis d
                display dis e
                display dis f
                display dis g
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

loop :: DisplayState () -> DisplayState ()
loop constr = do
    event <- liftIO SDL.waitEvent
    case SDL.eventPayload event of
        SDL.KeyboardEvent ke -> handleKeyPress ke
        SDL.WindowResizedEvent e -> handleWinResize e
        SDL.QuitEvent -> return ()
        _ -> present >> loop constr
    where
    handleWinResize :: SDL.WindowResizedEventData -> DisplayState ()
    handleWinResize (SDL.WindowResizedEventData win size) = do
            modDisplay $ \disp -> if win == window disp
                 then disp { windowSize = size }
                 else disp
            loop constr
    handleKeyPress :: SDL.KeyboardEventData -> DisplayState ()
    handleKeyPress (SDL.KeyboardEventData _win _mot _rep keysym) =
            case SDL.unwrapKeycode $ SDL.keysymKeycode keysym of
                13 -> constr >> present >> loop constr
                27 -> return () -- ESC
                _ -> loop constr

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
    V2 (CInt winx) (CInt winy) <- SDL.get $ SDL.windowSize win
    return $ Display {
        window = win,
        windowSize = V2 winx winy,
        renderer = renderer,
        font = font,
        draw = draw
    }


main :: IO ()
main = do
    SDL.initialize [ SDL.InitEvents, SDL.InitVideo ]
    Font.initialize
    constr <- getArgs >>= selectConstruction
    display <- initView >>= runDisplay (loop constr)
    Font.free $ font display
    Font.quit
    SDL.quit
