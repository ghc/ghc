{-# LANGUAGE BangPatterns, MagicHash #-}
-- A loop that allocates on every iteration but scrutinises only unboxed
-- values, so every jump stays in the defining BCO: the only way out of the
-- BCO -- and so the only way to a GC -- is the safepoint on the back edge
-- (YIELD_CHECK -> slow path -> closure entry -> heap check). A rise in the
-- GC count while this loop runs is therefore evidence that the slow path
-- ran; a loop whose safepoint never fired could add at most about two GCs
-- (at the first BCO entry after the loop), while this one adds about 17 at
-- -A1m. See Note [Join points as loops] in StgToByteCode.hs.
-- The ghci way of the testsuite puts its own -O0 after the test's -O; this
-- pragma is applied per module, after the command line, so the loops are
-- compiled at -O in both ghci ways.
{-# OPTIONS_GHC -O #-}
module LoopSlow (grow) where

import GHC.Exts

grow :: Int -> Int
grow n0 = go (case n0 of I# n -> n) []
  where
    go :: Int# -> [Int] -> Int
    go 0# xs = length xs + n0
    go k xs = case andI# k 63# of
      0# -> go (k -# 1#) (I# k : xs)
      _  -> go (k -# 1#) (I# (k +# 1#) : xs)
{-# NOINLINE grow #-}
