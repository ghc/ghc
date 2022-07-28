{-# OPTIONS_GHC -fno-worker-wrapper #-}

-- The -fno-worker-wrapper stops f1, f2 etc from worker/wrappering
-- via CPR analysis, after which they inline ane confuse the
-- detection of eta-expansion or otherwise

module T21261 where

-- README: The convention here is that bindings starting with 'yes' should be
-- eta-reduced and become trivial, while bindings starting with 'no' should not
-- be eta-reduced.

f1 :: (Int -> Int -> Int) -> Int
f1 c = c 1 2 + c 3 4
{-# NOINLINE f1 #-}
yes1 :: (Int -> Int -> Int) -> Int
yes1 c = f1 (\x -> c x)

f2 :: (Int -> Int -> Int) -> Int
f2 c = c 1 `seq` c 3 4
{-# NOINLINE f2 #-}
yes1or2 :: (Int -> Int -> Int) -> Int
yes1or2 c = f2 c

f3 :: (Int -> Int -> Int) -> Int
f3 c = c 1 2 + c 3 4
{-# NOINLINE f3 #-}
yes2 :: (Int -> Int -> Int) -> Int
yes2 c = f3 (\x y -> c x y)

f4 :: (Int -> Int -> Int -> Int) -> Int
f4 c = c 1 2 `seq` c 3 4 `seq` 42
{-# NOINLINE f4 #-}
no3 :: (Int -> Int -> Int -> Int) -> Int
no3 c = f4 (\x y z -> c x y z)

f5 :: (Int -> Int -> Int) -> Maybe Int
f5 c = Just (c 1 2 + c 3 4)
{-# NOINLINE f5 #-}
yes2_lazy :: (Int -> Int -> Int) -> Maybe Int
yes2_lazy c = f5 (\x y -> c x y)

-- These last two here are tricky. It is unsound to eta reduce
-- the args to f6 and f7 because we could call both functions
-- at a c like `\x -> if x == 1 then undefined else id`.
-- Note that if we do so and *do* eta-reduce, we'll see a crash!
-- Whereas we *don't* see a crash if we keep the original,
-- eta-expanded arguments.
--
-- This is issue is discussed in Note [Eta reduction soundness], condition (S).
--
-- This is a variant of #21717 (and tested by T21717), where
-- prior to the fix we observed an unsound strictness signature.

f6 :: (Int -> Int -> Int) -> Int
f6 c = c 1 `seq` c 2 3
{-# NOINLINE f6 #-}
no_tricky :: (Int -> Int -> Int) -> Int
no_tricky c = f6 (\x y -> c x y)

f7 :: (Int -> Int -> Int) -> Maybe Int
f7 c = Just (c 1 `seq` c 3 4)
{-# NOINLINE f7 #-}
no_tricky_lazy :: (Int -> Int -> Int) -> Maybe Int
no_tricky_lazy c = f7 (\x y -> c x y)
