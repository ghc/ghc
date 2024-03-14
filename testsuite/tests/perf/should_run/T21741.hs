{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment

f :: [Int] -> Int
f xs = g (length xs) (even $ mySum xs)
{-# NOINLINE f #-}

g :: Int -> Bool -> Int
g 0 _ = 0
g n !b = length xs + mySum xs + if b then 0 else 1
  where
    xs = [0..n]
{-# NOINLINE g #-}

mySum :: [Int] -> Int
mySum = go 0
  where
    go acc (x:xs) = go (x+acc) xs
    go acc _      = acc

main = do
  (n:_) <- map read <$> getArgs
  print $ f [0..n]
