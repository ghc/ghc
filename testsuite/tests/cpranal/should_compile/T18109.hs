{-# OPTIONS_GHC -O2 -fforce-recomp -dno-typeable-binds #-}

-- | These are all examples where the CPR worker should not return an unboxed
-- singleton tuple of the field, but rather the single field directly.
-- This is OK if the field indeed terminates quickly;
-- see Note [No unboxed tuple for single, unlifted transit var]
module T18109 where

data F = F (Int -> Int)

f :: Int -> F
f n = F (+n)
{-# NOINLINE f #-}

data T = T (Int, Int)

g :: T -> T
g t@(T p) = p `seq` t
{-# NOINLINE g #-}

data U = U [Int]

h :: U -> U
h u@(U xs) = xs `seq` u
{-# NOINLINE h #-}
