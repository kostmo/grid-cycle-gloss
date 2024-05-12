module Flyp.Util where

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

{- | Take the successor of an 'Enum' type, wrapping around when it
  reaches the end.
-}
cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
    | e == maxBound = minBound
    | otherwise = succ e
