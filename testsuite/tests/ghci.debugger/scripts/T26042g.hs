module T9 where

import Prelude hiding (succ)

top = do
  case succ 14 of
    5 -> 5
    _ -> 6 + other 55

succ :: Int -> Int
succ x = (-) (x - 2) (x + 1)

other :: Int -> Int
other x = x * 3
{-# OPAQUE other #-}
