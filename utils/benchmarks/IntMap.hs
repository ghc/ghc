{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import System.Event.IntMap (IntMap)
import qualified System.Event.IntMap as IM

main = defaultMain
    [ bench "insert10k" $ whnf ascFrom n
    ]
  where
    -- Number of elements
    n = 10000

-- | Create an integer map with keys in ascending order starting at 0
-- and ending at @max@ (exclusive.)
ascFrom :: Int -> IntMap Int
ascFrom max = go 0 IM.empty
  where
    go :: Int -> IntMap Int -> IntMap Int
    go n !mp
        | n >= max  = mp
        | otherwise = let (_, !mp') = IM.insertWith const n n mp
                      in go (n + 1) mp'
