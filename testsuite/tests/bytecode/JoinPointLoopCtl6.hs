-- A self-recursive join point whose body jumps all sit in one continuation
-- BCO: the expected JoinRejectLoopInCont.
--
-- 'g' is NOINLINE, so the case on it is a real continuation and both
-- alternatives jump to 'go' from inside it. Two jumps, not one, is what keeps
-- FloatIn from pushing the binding into an alternative -- with a single use it
-- would move inside and the label would be at the empty path again, which is
-- Loop1.
--
-- Neither alternative may force anything on the way to its jump: a forced
-- boxed value is another continuation, which puts the two jumps at different
-- paths and makes this a spread-chain rejection instead. The first version of
-- this module passed 'm :: Int' in one alternative and did exactly that; the
-- jumps take literals now.
--
-- 'go' mentions 'n' so that FloatOut cannot lift it to the top level.
module JoinPointLoopCtl6 (f) where

f :: Int -> Int
f n = case g n of
        0 -> go 7 0
        k -> go k 1
  where
    go :: Int -> Int -> Int
    go 0 a = a + n
    go k a = go (k - 1) (a + k)
{-# NOINLINE f #-}

g :: Int -> Int
g x = x + 1
{-# NOINLINE g #-}
