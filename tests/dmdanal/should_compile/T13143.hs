module T13143 where

{-# NOINLINE f #-}
f :: Int -> a
f x = f x

g :: Bool -> Bool -> Int -> Int
g True  True  p = f p
g False True  p = p + 1
g b     False p = g b True p
