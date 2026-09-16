-- A self-recursive join point reached only from inside a case continuation:
-- the expected JoinAsLoop with a non-empty ContPath, if the jump in the RHS
-- is at the empty path relative to the label.
module JoinPointLoopCtl2 (f) where

f :: [Int] -> Int -> Int
f xs n = case sum xs of
  s | s > 0 -> go n s
    | otherwise -> negate s
  where
    go :: Int -> Int -> Int
    go 0 a = a + n
    go k a = go (k - 1) (a + k)
{-# NOINLINE f #-}
