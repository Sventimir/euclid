{-# LANGUAGE OverloadedStrings #-}
module Display (
    Display(..),
    DisplayState(..),
    runDisplay,
    getDisplay,
    modDisplay,
    display
) where

import Control.Monad.State
import Foreign.C.Types (CInt(..))
import Linear (V2(..))
import qualified SDL.Vect as SDL
import qualified SDL.Font as Font
import SDL.Video (Window, Renderer, drawPoint, drawLine)

import Figure


toPixels :: V2 Double -> V2 CInt
toPixels (V2 x y) = V2 (CInt $ round (x * 100)) (CInt $ round (y * 100))

data Display = Display {
    window :: Window,
    renderer :: Renderer,
    font :: Font.Font,
    draw :: IO ()
}

toSDLPoint :: Point -> SDL.Point V2 CInt
toSDLPoint = SDL.P . toPixels . loc


class Displayable d where
    display :: Display -> d -> IO ()

instance Displayable Point where
    display disp p = do
        drawPoint (renderer disp) $ toSDLPoint p

instance Displayable Segment where
    display disp (Segment begin end) = do
        drawLine (renderer disp) (toSDLPoint begin) (toSDLPoint end)

instance Displayable Circle where
    display disp c = do
            let V2 x y = loc $ center c
            display disp $ center c
            sequence_ $ drawCircumference x y (radius c)
        where
        y_of_x x = sqrt ((radius c) ** 2 - x ** 2)
        drawCircumference x y r =
            let circumference = round (2 * pi * r * 100)
                angles = [2 * pi * fromIntegral x / fromIntegral circumference | x <- [0..circumference]]
            in
            map (\a ->
                drawPoint (renderer disp) . SDL.P . toPixels $ V2 (cos a * r + x) (sin a * r + y)
            ) angles

newtype DisplayState a = DisplayState (StateT Display IO a)

runDisplay :: DisplayState a -> Display -> IO Display
runDisplay (DisplayState s) init = execStateT s init

instance Functor DisplayState where
    fmap f (DisplayState s) = DisplayState (fmap f s)

instance Applicative DisplayState where
    pure a = DisplayState $ pure a
    (<*>) (DisplayState mf) (DisplayState ma) =
            DisplayState (mf >>= \f -> ma >>= \a -> return (f a))

instance Monad DisplayState where
    return = pure
    (>>=) (DisplayState s) f =
            DisplayState $ s >>= ((\(DisplayState s') -> s') . f)

instance MonadIO DisplayState where
    liftIO m = DisplayState (lift m)

getDisplay :: DisplayState Display
getDisplay = DisplayState get

modDisplay :: (Display -> Display) -> DisplayState ()
modDisplay f = DisplayState $ modify f
