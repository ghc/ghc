-- Unboxed counterparts to data structures

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}

{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
  -- If you use -fomit-interface-pragmas for your build, we won't
  -- inline the matcher for JustUB, and that turns out to have a
  -- catastropic effect on Lint, which uses unboxed Maybes.
  -- Simple fix: switch off -fomit-interface-pragmas for this tiny
  -- and very stable module.

module GHC.Data.Unboxed (
  MaybeUB(JustUB, NothingUB),
  fmapMaybeUB, fromMaybeUB, apMaybeUB, maybeUB
  ) where

import GHC.Prelude hiding (Maybe(..), Either(..))

-- | Like Maybe, but using unboxed sums.
--
-- Use with care. Using a unboxed maybe is not always a win
-- in execution *time* even when allocations go down. So make
-- sure to benchmark for execution time as well. If the difference
-- in *runtime* for the compiler is too small to measure it's likely
-- better to use a regular Maybe instead.
--
-- This is since it causes more function arguments to be passed, and
-- potentially more variables to be captured by closures increasing
-- closure size.
newtype MaybeUB a = MaybeUB (# (# #) | a #)

pattern JustUB :: a -> MaybeUB a
pattern JustUB x = MaybeUB (# | x #)

pattern NothingUB :: MaybeUB a
pattern NothingUB = MaybeUB (# (# #) | #)

{-# COMPLETE NothingUB, JustUB #-}

fromMaybeUB :: a -> MaybeUB a -> a
fromMaybeUB d NothingUB = d
fromMaybeUB _ (JustUB x) = x

apMaybeUB :: MaybeUB (a -> b) -> MaybeUB a -> MaybeUB b
apMaybeUB (JustUB f) (JustUB x) = JustUB (f x)
apMaybeUB _ _ = NothingUB

fmapMaybeUB :: (a -> b) -> MaybeUB a -> MaybeUB b
fmapMaybeUB _f NothingUB = NothingUB
fmapMaybeUB f (JustUB x) = JustUB $ f x

maybeUB :: b -> (a -> b) -> MaybeUB a -> b
maybeUB _def f (JustUB x) = f x
maybeUB def _f NothingUB = def
