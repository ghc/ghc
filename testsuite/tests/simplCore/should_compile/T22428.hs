module T22428 where

f :: Integer -> Integer -> Integer
f x y = go y
  where
    go :: Integer -> Integer
    go 0 = x
    go n = go (n-1)
    {-# INLINE go #-}
