{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A tiny wrapper around 'IntSet.IntSet' for representing sets of 'Enum'
-- things.
module GHC.Data.EnumSet
    ( EnumSet
    , member
    , insert
    , delete
    , toList
    , fromList
    , empty
    , difference
    ) where

import GHC.Prelude
import GHC.Utils.Binary

import qualified Data.IntSet as IntSet

newtype EnumSet a = EnumSet IntSet.IntSet
  deriving (Semigroup, Monoid)

member :: Enum a => a -> EnumSet a -> Bool
member x (EnumSet s) = IntSet.member (fromEnum x) s

insert :: Enum a => a -> EnumSet a -> EnumSet a
insert x (EnumSet s) = EnumSet $ IntSet.insert (fromEnum x) s

delete :: Enum a => a -> EnumSet a -> EnumSet a
delete x (EnumSet s) = EnumSet $ IntSet.delete (fromEnum x) s

toList :: Enum a => EnumSet a -> [a]
toList (EnumSet s) = map toEnum $ IntSet.toList s

fromList :: Enum a => [a] -> EnumSet a
fromList = EnumSet . IntSet.fromList . map fromEnum

empty :: EnumSet a
empty = EnumSet IntSet.empty

difference :: EnumSet a -> EnumSet a -> EnumSet a
difference (EnumSet a) (EnumSet b) = EnumSet (IntSet.difference a b)

-- | Represents the 'EnumSet' as a bit set.
--
-- Assumes that all elements are non-negative.
--
-- This is only efficient for values that are sufficiently small,
-- for example in the lower hundreds.
instance Binary (EnumSet a) where
  put_ bh = put_ bh . enumSetToBitArray
  get bh = bitArrayToEnumSet <$> get bh

-- TODO: Using 'Natural' instead of 'Integer' should be slightly more efficient
-- but we don't currently have a 'Binary' instance for 'Natural'.
type BitArray = Integer

enumSetToBitArray :: EnumSet a -> BitArray
enumSetToBitArray (EnumSet int_set) =
    IntSet.foldl' setBit 0 int_set

bitArrayToEnumSet :: BitArray -> EnumSet a
bitArrayToEnumSet ba = EnumSet (go (popCount ba) 0 IntSet.empty)
  where
    go 0 _ !int_set = int_set
    go n i !int_set =
      if ba `testBit` i
        then go (pred n) (succ i) (IntSet.insert i int_set)
        else go n        (succ i) int_set
