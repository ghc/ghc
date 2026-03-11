{-# LANGUAGE MagicHash, UnboxedTuples, BlockArguments #-}
module Main where

import GHC.Exts
import GHC.ST   (ST(..), runST)
import System.Mem (performMajorGC)

-- Lifted wrapper so SmallArray# can appear in non-unlifted positions.
data SmallArr a = SmallArr (SmallArray# a)

main :: IO ()
main = do
  let arr = buildArr
  let n = case arr of SmallArr a -> I# (sizeofSmallArray# a)
  putStrLn $ "size after shrink = " ++ show n
  -- With +RTS -hT -i0 this triggers heapCensus.
  -- The census advances past the 10 live elements, then hits the 490
  -- stale heap pointers in the slop and crashes.
  performMajorGC
  -- Keep arr alive across the GC by reading from it after.
  let v = case arr of SmallArr a -> case indexSmallArray# a 0# of (# x #) -> x
  putStrLn $ "arr[0] = " ++ show (v :: Integer)
  putStrLn "survived"

-- Allocate 500 slots, write a DISTINCT Integer to every slot, then
-- shrink to 10.  Slots [10..499] become slop: they still hold live,
-- non-zero heap pointers in the raw memory, but the ptrs header field
-- says there are only 10 elements.  zeroSlop is a no-op in non-PROFILING
-- builds (even when -hT is active), so the slop is never cleared.
buildArr :: SmallArr Integer
buildArr = runST $ ST \s0 ->
  -- All 500 slots start with a non-null initial value.
  case newSmallArray# 500# (0 :: Integer) s0 of { (# s1, ma #) ->
  -- Overwrite every slot with a distinct Integer so each holds a
  -- unique heap pointer (no sharing, definitely non-zero).
  case fill 499 ma s1 of { s2 ->
  -- Shrink: ptrs = 10, but physical slots [10..499] are NOT zeroed.
  case shrinkSmallMutableArray# ma 10# s2 of { s3 ->
  case unsafeFreezeSmallArray# ma s3 of { (# s4, a #) ->
  (# s4, SmallArr a #) }}}}

-- Fill slots [0..n] each with a distinct Integer value (n, n-1, ..., 0).
-- 'Integer' guarantees a genuine heap object for every value.
fill :: Int -> SmallMutableArray# s Integer -> State# s -> State# s
fill 0 ma s = writeSmallArray# ma 0# (0 :: Integer) s
fill n ma s =
  let I# n# = n
  in case writeSmallArray# ma n# (fromIntegral n :: Integer) s of
       s' -> fill (n - 1) ma s'
