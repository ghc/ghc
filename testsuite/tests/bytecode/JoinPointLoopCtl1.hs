-- A self-recursive join point whose jumps are all in the defining BCO:
-- the expected JoinAsLoop (ContPath []).
--
-- 'go' mentions 'n' so that FloatOut cannot lift it to the top level, which
-- is what stops a closed local loop from being a join point at all.
module JoinPointLoopCtl1 (f) where

f :: Int -> Int -> Int
f n acc = go n acc
  where
    go :: Int -> Int -> Int
    go 0 a = a + n
    go k a = go (k - 1) (a + k)
{-# NOINLINE f #-}
