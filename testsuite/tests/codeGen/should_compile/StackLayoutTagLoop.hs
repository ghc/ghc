{-# LANGUAGE BangPatterns #-}
module StackLayoutTagLoop (sumWith) where

-- The loop evaluates each element in a diamond, a tag test with a call
-- returning to a continuation, k, acc and xs live across it.  Stack layout
-- keeps the stack's depth across the loop and the diamond's frame within it
-- (Note [Loop head stack maps], Note [Diamond frames keep their depth] in
-- GHC.Cmm.LayoutStack), so no return address is stored below Sp: before,
-- every iteration set the frame up below Sp, moved Sp down and back up,
-- and stored k, acc and xs again on the evaluated path too.
sumWith :: Int -> Int -> [Int] -> Int
sumWith !k !acc []       = acc
sumWith !k !acc (x : xs) = sumWith k (acc + k * x) xs
{-# NOINLINE sumWith #-}
