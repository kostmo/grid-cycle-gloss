module Flyp where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (interactIO)

import Flyp.Draw
import Flyp.Event
import Flyp.State

go :: IO ()
go = do
    putStrLn "Press Esc with the game window focused to exit."
    interactIO
        (InWindow "Grid game" (800, 600) (200, 200))
        white
        initState
        drawPicture
        handleEvent
        (const $ return ())
