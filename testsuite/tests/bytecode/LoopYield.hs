{-# LANGUAGE MagicHash #-}
-- A loop with an unboxed and a boxed parameter live across its safepoint,
-- allocating on every iteration so the safepoint fires: the shape of
-- LoopSlow.hs with a boxed accumulator added that the loop only ever passes
-- on, and that is read after the loop has ended.
--
-- This is the shape that catches a mistake in the bitmap of the resume frame
-- the safepoint leaves behind (see Note [Join points as loops] in
-- StgToByteCode.hs). At the safepoint the loop's frame holds the counter as a
-- raw Int# and 'best' and 'xs' as pointers. Describe the counter as a pointer
-- and +RTS -DS follows it at the first collection during the loop; describe
-- 'best' as a non-pointer and it is not updated when the collector moves it,
-- so the sum printed at the end is wrong or worse. 'best' is rebuilt inside
-- the loop, so at any safepoint it is a young object that a collection is
-- very likely to move.
--
-- The ghci way of the testsuite puts its own -O0 after the test's -O; this
-- pragma is applied per module, after the command line, so the loop is
-- compiled at -O in both ghci ways.
{-# OPTIONS_GHC -O #-}
module LoopYield (walk) where

import GHC.Exts

walk :: Int -> Int
walk n0 = go (case n0 of I# n -> n) (I# 0#) []
  where
    go :: Int# -> Int -> [Int] -> Int
    go 0# best xs = length xs + n0 + best
    go k best xs = case andI# k 1023# of
      0# -> go (k -# 1#) (I# k) (I# k : xs)
      _  -> go (k -# 1#) best   (I# (k +# 1#) : xs)
{-# NOINLINE walk #-}
