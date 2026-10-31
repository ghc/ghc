{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module T8905 (sumElems) where

import GHC.Exts

-- #8905: each element's evaluation tests the element's tag, and only an
-- unevaluated element is entered.  The tag test went to the same label as the
-- call's return, so the frame for the call, the return address and the values
-- live across it, was set up before the test, on the evaluated path too; here
-- at every element of the loop.  The test checks that the tag test precedes
-- the copy of the element into R1 for the call, so that only the call's path
-- sets up the frame.

data A = A (Array# Double)

unI :: Int -> Int#
unI (I# x) = x

sumElems :: A -> Int -> Double
sumElems (A arr) off = go 0 0
  where
    go !i !acc
      | i >= 500 = acc
      | otherwise = case indexArray# arr (unI (off + i)) of
          (# x #) -> go (i + 1) (acc + x)
