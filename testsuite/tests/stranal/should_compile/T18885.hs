{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module T18885 where

expensive :: Int -> Int
expensive n = sum [0..n]
{-# NOINLINE expensive #-}

f :: Int -> Int
f y =
  let x
       | expensive y == 1 = (expensive (y+1), expensive (y+2))
       | otherwise        = (expensive (y+3), expensive (y+4))
  in case () of
       _ | expensive (y+5) == 42 -> fst x
       _ | expensive (y+6) == 41 -> fst x + snd x
       _ | otherwise             -> 0
