{-# OPTIONS_GHC -O -fno-call-arity -fforce-recomp #-}

module T18894 (f) where

expensive :: Int -> (Int, Int)
expensive n = (n+1, n+2)
{-# NOINLINE expensive #-}

-- arity 1 by itself, but not exported, thus can be eta-expanded based on usage
eta :: Int -> Int -> Int
eta x = if fst (expensive x) == 13
           then \y -> x + y
           else \y -> x * y
{-# NOINLINE eta #-}

f :: Int -> Int
f 1 = 0
f m
  | odd m     = eta m 2
  | otherwise = eta 2 m
