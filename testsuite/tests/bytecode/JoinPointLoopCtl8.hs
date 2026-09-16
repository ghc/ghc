-- The negative control for JoinPointLoopCtl7: a join point jumped to from
-- inside a loop's RHS that is NOT placed.
--
-- 'j' is jumped to from inside the loop's RHS, as in JoinPointLoopCtl7, but
-- its other two jumps sit in two sibling continuation BCOs -- 'opaque' is
-- NOINLINE, so each alternative's case on it is a real continuation. Looking
-- through the loop's RHS turns the occurrence inside it into a jump at the
-- empty path and leaves the spread, so 'j' is rejected as spread-siblings.
-- This is what says that 'in-loop-join' reaching 0 is the occurrence being
-- reclassified, not every such join point being won.
{-# LANGUAGE BangPatterns #-}
module JoinPointLoopCtl8 (f) where

opaque :: Int -> Int
opaque x = x + 1
{-# NOINLINE opaque #-}

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
       0 -> case opaque acc of s -> j s
       1 -> case opaque n of t -> j t
       _ -> go n acc
{-# NOINLINE f #-}
