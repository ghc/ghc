module T18793 where

stuff :: Int -> [Int]
{-# NOINLINE stuff #-}
stuff i = [i,i+1,i+2]

f :: Int -> Int
f = foldr k id (stuff 1)
  where
    k :: Int -> (Int -> Int) -> (Int -> Int)
    k i acc | i > 42    = acc . negate
            | otherwise = acc
