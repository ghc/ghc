{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import GHC.Event.PSQ (PSQ)
import qualified GHC.Event.PSQ as Q

main = defaultMain
    [ bench "atMost1k/length" $ whnf (atMostLength 1000) q
    , bench "insert10k/min" $ whnf (Q.findMin . ascFrom) n
    , bench "delete1k/min" $ whnf (Q.findMin . deleteEveryN (n `div` 1000) n) q
    , bench "adjust1k/min" $ whnf (Q.findMin . adjustEveryN (n `div` 1000) n) q
    ]
  where
    -- Number of elements
    n = 10000

    -- Priority queue with 'n' elements
    q = ascFrom n

-- | Return the number of elements with priority at most @pt@
atMostLength :: Q.Prio -> PSQ Int -> Int
atMostLength pt q = length . fst . Q.atMost pt $ q

-- | Create a priority queue with keys and priorities in ascending
-- order starting at 0 and ending at @max@ (exclusive.)
ascFrom :: Int -> PSQ Int
ascFrom max = go 0 Q.empty
  where
    go :: Int -> PSQ Int -> PSQ Int
    go n !q
        | n >= max  = q
        | otherwise = go (n + 1) $
                      Q.insert (fromIntegral n) (fromIntegral n) n q

-- | Delete all keys that are multiples of @step@ but less than @max@.
deleteEveryN :: Int -> Int -> PSQ a -> PSQ a
deleteEveryN step max q0 = go 0 q0
  where
    go :: Int -> PSQ a -> PSQ a
    go n !q
        | n >= max  = q
        | otherwise = go (n + step) $ Q.delete (fromIntegral n) q

-- | Adjust the priority of all keys that are multiples of @step@ but
-- less than @max@.
adjustEveryN :: Int -> Int -> PSQ a -> PSQ a
adjustEveryN step max q0 = go 0 q0
  where
    go :: Int -> PSQ a -> PSQ a
    go n !q
        | n >= max  = q
        | otherwise = go (n + step) $ Q.adjust (+ 1) (fromIntegral n) q
