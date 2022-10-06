{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module T22277 where

entry :: Int -> Int
entry n = case n of
  0 -> f n (13,24)
  _ -> f n (n,n)
  where
    f :: Int -> (Int,Int) -> Int
    f m x = g m x
      where
        exit m = (length $ reverse $ reverse $ reverse $ reverse $ [0..m]) + n
        g n p | even n    = exit n
              | n > 43    = g (n-1) p
              | otherwise = fst p
