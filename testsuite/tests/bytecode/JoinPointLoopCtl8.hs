-- The negative control for in-loop-join-placeable: an in-loop-join join point
-- that a fallback-free loop would NOT place.
--
-- 'j' is jumped to from inside the loop's RHS, as in JoinPointLoopCtl7, but its other two
-- jumps sit in two sibling continuation BCOs -- 'opaque' is NOINLINE, so each
-- alternative's case on it is a real continuation. 'singleBco' reports the
-- occurrence in the closure before it looks at the spread, so the verdict
-- today is in-loop-join like JoinPointLoopCtl7's; assuming the fallback away turns that
-- occurrence into a jump at the empty path and leaves the spread, so the
-- counterfactual verdict is a cross-BCO rejection and the counter stays 0.
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
