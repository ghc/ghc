-- A join point jumped to from inside a loop's RHS: a label, since a loop's
-- RHS is emitted once, where its label is.
--
-- 'j' is jumped to from two places, one in the BCO that defines it and one in
-- the RHS of the loop 'go'. Both are the same BCO, so 'j' is placed there by
-- the ordinary rule and 'in-loop-join' is 0. (It used to be 1: the loop's RHS
-- was compiled a second time into a fallback closure, which made the second
-- jump an occurrence in a closure.)
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
