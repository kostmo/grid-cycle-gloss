module Flyp.Event where

import qualified Data.Vector as V
import Graphics.Gloss.Interface.IO.Interact
import System.Exit (exitSuccess)

import Flyp.Draw
import Flyp.State
import Flyp.Util

clampSingleGridBound :: Int -> Int -> Int
clampSingleGridBound d = min (d - 1) . max 0

clampGridBounds :: (Int, Int) -> (Int, Int)
clampGridBounds (x, y) =
    (clampSingleGridBound boardW x, clampSingleGridBound boardH y)
  where
    BoardDimensions boardW boardH = boardSize

getGridCoord :: (Float, Float) -> (Int, Int)
getGridCoord (x, y) =
    clampGridBounds (adj boardW x, adj boardH y)
  where
    BoardDimensions boardW boardH = boardSize
    adj :: Int -> Float -> Int
    adj d val =
        floor $ val / (separation * unitScale) + fromIntegral d / 2

modifyAt :: (t -> t) -> Int -> V.Vector t -> V.Vector t
modifyAt f idx oldVec =
    oldVec V.// [(idx, f $ oldVec V.! idx)]

cycleSlot ::
    (Int, Int) ->
    V.Vector (V.Vector Slot) ->
    V.Vector (V.Vector Slot)
cycleSlot (x, y) = modifyAt (modifyAt cycleEnum x) y

data ArrowDir = ArrDown | ArrUp | ArrLeft | ArrRight

moveActive :: ArrowDir -> MyWorld -> IO MyWorld
moveActive d (MyWorld (InteractionState i (x, y)) slotMembers) =
    return $ MyWorld (InteractionState i $ clampGridBounds newCoords) slotMembers
  where
    newCoords = case d of
        ArrDown -> (x, y - 1)
        ArrUp -> (x, y + 1)
        ArrLeft -> (x - 1, y)
        ArrRight -> (x + 1, y)

handleEvent :: Event -> MyWorld -> IO MyWorld
handleEvent e s@(MyWorld iState slotMembers) = case e of
    EventKey (MouseButton LeftButton) Down _ coords@(x, y) -> do
        putStrLn $
            unwords
                [ "X:"
                , show x
                , "Y:"
                , show y
                , "Grid:"
                , show gc
                ]
        return $ MyWorld (InteractionState i gc) $ cycleSlot gc slotMembers
      where
        gc = getGridCoord coords
        InteractionState i _clickCoord = iState
    EventKey (SpecialKey KeyEnter) Down _ _ ->
        return $ MyWorld (InteractionState (i + 1) clickCoord) slotMembers
      where
        InteractionState i clickCoord = iState
    EventKey (SpecialKey KeySpace) Down _ _ ->
        return $
            MyWorld (InteractionState i clickCoord) $
                cycleSlot clickCoord slotMembers
      where
        InteractionState i clickCoord = iState
    EventKey (SpecialKey KeyDown) Down _ _ -> moveActive ArrDown s
    EventKey (SpecialKey KeyUp) Down _ _ -> moveActive ArrUp s
    EventKey (SpecialKey KeyLeft) Down _ _ -> moveActive ArrLeft s
    EventKey (SpecialKey KeyRight) Down _ _ -> moveActive ArrRight s
    EventKey (Char 'q') Down _ _ -> exitSuccess
    EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess
    _ -> return s

handleSim :: Float -> MyWorld -> MyWorld
handleSim _ w = w
