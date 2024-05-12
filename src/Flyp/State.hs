module Flyp.State where

import Data.Vector (Vector)
import qualified Data.Vector as V

data BoardDimensions = BoardDimensions
    { boardWidth :: Int
    , boardHeight :: Int
    }

boardSize :: BoardDimensions
boardSize = BoardDimensions 7 5

data Slot
    = Green
    | Yellow
    | Red
    deriving (Eq, Enum, Bounded)

data InteractionState = InteractionState
    { toggleCount :: Int
    , lastClickCoords :: (Int, Int)
    }

data MyWorld = MyWorld
    { interactionState :: InteractionState
    , slots :: Vector (Vector Slot)
    }

initState :: MyWorld
initState =
    MyWorld iState $
        V.replicate (boardHeight boardSize) $
            V.replicate (boardWidth boardSize) Red
  where
    iState = InteractionState 0 (0, 0)
