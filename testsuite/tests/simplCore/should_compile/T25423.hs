{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module T25423 where

f :: Int -> Int -> (# (# Int, Int #), (# Int, Int #) #)
f x y = let !p = (# x, y #) in (# p, p #)
