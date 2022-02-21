module T21128 where

import T21128a

theresCrud :: Int -> Int -> Int
theresCrud x y = go x
  where
    go 0 = index 0 y 0
    go 1 = index x y 1
    go n = go (n-1)
{-# NOINLINE theresCrud #-}
