module Flyp where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Graphics.Gloss.Interface.IO.Interact (interactIO)
import Data.Tuple.Extra (both)

import Flyp.Draw
import Flyp.Event
import Flyp.State

go :: IO ()
go = do
    putStrLn "Press Esc with the game window focused to exit."
    interactIO
        (InWindow "Grid game" windowSize (200, 200))
        white
        initState
        drawPicture
        handleEvent
        (const $ return ())
  where
    windowSize = both floor scaledSize
    scaledSize = (separation * unitScale) A.* both fromIntegral (x, y)
    BoardDimensions x y = boardSize
