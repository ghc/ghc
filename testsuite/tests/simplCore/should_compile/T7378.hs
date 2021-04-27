module T7378 where

-- The original example, now compiled to a simple right-shift
fun0 :: Int -> Int
fun0 i = i `div` 2

-- We expect CSE to combine the alternatives of a redundant
-- case analysis on `i > 0`.
-- This didn't happen before GHC 8.2.
fun1 :: Int -> Int
fun1 i = i `div` 12345
