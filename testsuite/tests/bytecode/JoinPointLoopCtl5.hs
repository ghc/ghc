-- The ordering guard. The RHS both defines a placed join point AND has a
-- recursive jump in a continuation BCO, so it fails the jump-site condition
-- as well as the placement rule. It must be counted as rejected-recursive,
-- not as loop-rejected-placed-join: the placement counter exists to price
-- what lifting the double-compilation rule would win, and a join point that
-- is rejected for its jumps anyway would not be won.
{-# LANGUAGE BangPatterns #-}
module JoinPointLoopCtl5 (f) where

opaque :: Int -> Int
opaque x = x + 1
{-# NOINLINE opaque #-}

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
           -- the scrutinee is a call, so this alternative's jump to 'go' is
           -- emitted into a continuation BCO
           2 -> case opaque a of s -> go (k - 1) (a + s)
           _ -> go (k - 1) (a + k)
{-# NOINLINE f #-}
