module T21128 where

import T21128a

{- This test originally had some unnecessary reboxing of y
in the hot path of $wtheresCrud.  That reboxing should
not happen. -}

theresCrud :: Int -> Int -> Int
theresCrud x y = go x
  where
    go 0 = index 0 y 0
    go 1 = index x y 1
    go n = go (n-1)
{-# NOINLINE theresCrud #-}

