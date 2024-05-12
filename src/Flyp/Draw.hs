module Flyp.Draw where

import qualified Data.Vector as V
import Graphics.Gloss

import Flyp.State
import Flyp.Util

separation :: Float
separation = 1.5

unitScale :: Float
unitScale = 40

drawTile :: InteractionState -> Int -> Int -> Slot -> Picture
drawTile iState colIdx rowIdx s =
    Pictures itemsToDraw
  where
    InteractionState _ clickCoord = iState

    itemsToDraw =
        applyWhen
            (clickCoord == (rowIdx, colIdx))
            (shadow :)
            [Color c $ rectangleSolid 1 1]

    shadow =
        Color (withAlpha 0.2 black) $
            rotate (-30) $
                scale 1.5 1 $
                    circleSolid 0.6

    c = case s of
        Green -> green
        Yellow -> yellow
        Red -> red

drawDimension ::
    Bool ->
    (BoardDimensions -> Int) ->
    (Int -> b -> Picture) ->
    V.Vector b ->
    Picture
drawDimension doFlip accessor renderer =
    transDim bulkOffset 0
        . Pictures
        . V.toList
        . V.imap f
  where
    bulkOffset = negate $ (fromIntegral (accessor boardSize - 1) * separation) / 2
    transDim = applyWhen doFlip flip translate
    f i x = transDim (fromIntegral i * separation) 0 $ renderer i x

drawRow :: InteractionState -> Int -> V.Vector Slot -> Picture
drawRow iState i = drawDimension False boardWidth $ drawTile iState i

drawRows :: InteractionState -> V.Vector (V.Vector Slot) -> Picture
drawRows iState = drawDimension True boardHeight $ drawRow iState

drawPicture :: MyWorld -> IO Picture
drawPicture (MyWorld iState xs) =
    return $
        scale unitScale unitScale $
            if isToggled
                then drawRows iState xs
                else Color blue $ thickCircle 2 0.5
  where
    isToggled = even $ toggleCount iState
