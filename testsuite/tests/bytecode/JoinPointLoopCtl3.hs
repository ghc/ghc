-- A join point that would be a loop -- every jump, including the recursive
-- one, in the BCO that would hold its label -- but whose RHS defines a join
-- point that is itself placed: the expected JoinRejectLoopPlacedJoin.
--
-- The recursive jump and both jumps to 'j' have to be in that same BCO, so
-- they sit in the alternatives of one case on an unboxed value, which is
-- compiled inline. The accumulator is strict for the same reason: forcing a
-- boxed Int on the way to a jump pushes a continuation BCO, and 'j' is then
-- rejected for spread jumps instead of being placed, which is not what this
-- module is testing.
{-# LANGUAGE BangPatterns #-}
module JoinPointLoopCtl3 (f) where

f :: Int -> Int -> Int -> Int
f n acc m = go n acc
  where
    go :: Int -> Int -> Int
    go k !a =
      let j :: Int -> Int
          j d = a + d + n + m
          {-# NOINLINE j #-}
      in case k of
           0 -> j 1
           1 -> j 2
           _ -> go (k - 1) (a + k)
{-# NOINLINE f #-}
