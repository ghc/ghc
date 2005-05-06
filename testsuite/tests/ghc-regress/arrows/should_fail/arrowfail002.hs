{-# OPTIONS -farrows #-}

module ShouldFail where

g :: Int -> Int
g = proc x -> f x -< x+1
  where f = (*)
