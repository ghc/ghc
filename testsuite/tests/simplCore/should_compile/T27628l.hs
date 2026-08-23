-- Max and Min (mimicking GHC.Internal.Data.Functor.Utils) both inline
-- base's foldl' and stimes templates, so each instance's methods bind
-- local loops with the SAME uniques as the other's.  The warnings must
-- stay separate per instance (keyed on (parent, function)), not be
-- fused into one with doubled callers; see Note [Reboxing warning].
module T27628l where

import Data.List (foldl')

newtype Max a = Max (Maybe a)

instance Ord a => Semigroup (Max a) where
    {-# INLINE (<>) #-}
    m <> Max Nothing = m
    Max Nothing <> n = n
    (Max m@(Just x)) <> (Max n@(Just y))
      | x >= y    = Max m
      | otherwise = Max n

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mconcat = foldl' (<>) mempty
    {-# INLINE mconcat #-}

newtype Min a = Min (Maybe a)

instance Ord a => Semigroup (Min a) where
    {-# INLINE (<>) #-}
    m <> Min Nothing = m
    Min Nothing <> n = n
    (Min m@(Just x)) <> (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mconcat = foldl' (<>) mempty
    {-# INLINE mconcat #-}
