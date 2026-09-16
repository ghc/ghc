-- A self-recursive join point whose recursive jump sits in a continuation BCO
-- of its own RHS: not at the empty path relative to the label, so it stays
-- JoinRejectRecursive.
module JoinPointLoopCtl4 (f) where

f :: [Int] -> Int -> Int
f xs acc = go xs acc
  where
    go :: [Int] -> Int -> Int
    go [] a = a
    go (y:ys) a = case sum (map (+ y) ys) of
      s -> go ys (a + s)
{-# NOINLINE f #-}
