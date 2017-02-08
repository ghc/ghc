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

f2 :: N -> Int
f2 n = case n of
  !(N {}) -> 0

f3 :: N -> Int
f3 n = n `seq` case n of
  N {} -> 0



main = do
  print $ f0 (trace "evaluated f0" (N 1))
  print $ f1 (trace "evaluated f1" (N 1))

  print $ f2 (trace "evaluated f2" (N 1))
  print $ f3 (trace "evaluated f3" (N 1))
