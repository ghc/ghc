module T22637 where

f :: Int -> Int
f x = x

{-# INLINE f #-}
{-# NOINLINE f #-}
