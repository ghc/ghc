module Lib (g) where

{-# RULES
  "blah" forall x . f x x = sf x
#-}

f :: Int -> Int -> (Int, Int)
f x y = (sum [0..x], sum [1..2*y])
{-# NOINLINE f #-}

sf :: Int ->  (Int, Int)
sf x = (sum [0..x], sum [1..2*x])
{-# NOINLINE sf #-}

g :: Int
g = case f 42 42 of (a, _) -> a + 1
