{-# LANGUAGE BangPatterns #-}
module Main where
import Debug.Trace

newtype N = N Int

f0 :: N -> Int
f0 n = case n of
  !(N _) -> 0

f1 :: N -> Int
f1 n = n `seq` case n of
  N _ -> 0

main = do
  print $ f0 (trace "evaluated f0" (N 1))
  print $ f1 (trace "evaluated f1" (N 1))
