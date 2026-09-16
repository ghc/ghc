-- A join point rejected for a jump inside a loop's RHS -- in-loop-join --
-- which would be a label if the loop had no fallback copy of its RHS: the
-- expected in-loop-join-placeable.
--
-- 'j' is jumped to from two places, one in the BCO that defines it and one in
-- the RHS of the loop 'go'. Today the second one is an occurrence in a closure
-- ('ClosureLoopJoin', since the loop's RHS is compiled a second time into the
-- fallback), so 'j' is rejected; assuming the fallback away, both jumps are in
-- the BCO of the definition and 'j' is a label.
--
-- The two call sites and the NOINLINE are what keep 'j' a join point rather
-- than an inlining; 'go' must not define any placed join point of its own, or
-- it would be loop-rejected-placed-join (Loop3) instead of a loop, and there
-- would be no loop RHS to look through.
{-# LANGUAGE BangPatterns #-}
module JoinPointLoopCtl7 (f) where

f :: Int -> Int -> Int -> Int
f n acc m =
  let j :: Int -> Int
      j d = d + n + m
      {-# NOINLINE j #-}
      go :: Int -> Int -> Int
      go k !a = case k of
                  0 -> j a
                  _ -> go (k - 1) (a + k)
  in case m of
       0 -> j acc
       _ -> go n acc
{-# NOINLINE f #-}
