{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace
import Text.Printf
import Prelude hiding (map)
import GHC.Stack

f :: Int -> Int
f x = traceStack (printf "f: %d" x) (x * 2)

map :: (a -> b) -> [a] -> [b]
map f xs = go xs
  where go [] = []
        go (x:xs) = f x : map f xs

main = do
  let xs = map f [42,43]
  print xs
  putStrLn =<< renderStack `fmap` (whoCreated $! head xs)

