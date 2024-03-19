{-# LANGUAGE UnboxedTuples #-}
module GHC.Data.FlatBag
  ( FlatBag
  , emptyFlatBag
  , unitFlatBag
  , sizeFlatBag
  , elemsFlatBag
  , mappendFlatBag
  -- * Construction
  , fromList
  , fromSizedSeq
  ) where

import GHC.Prelude

import GHC.Data.SizedSeq (SizedSeq, ssElts, sizeSS)

import Control.DeepSeq

import GHC.Data.SmallArray

-- | Store elements in a flattened representation.
--
-- A 'FlatBag' is a data structure that stores an ordered list of elements
-- in a flat structure, avoiding the overhead of a linked list.
-- Use this data structure, if the code requires the following properties:
--
-- * Elements are stored in a long-lived object, and benefit from a flattened
--   representation.
-- * The 'FlatBag' will be traversed but not extended or filtered.
-- * The number of elements should be known.
-- * Sharing of the empty case improves memory behaviour.
--
-- A 'FlagBag' aims to have as little overhead as possible to store its elements.
-- To achieve that, it distinguishes between the empty case, singleton, tuple
-- and general case.
-- Thus, we only pay for the additional three words of an 'Array' if we have at least
-- three elements.
data FlatBag a
  = EmptyFlatBag
  | UnitFlatBag !a
  | TupleFlatBag !a !a
  | FlatBag {-# UNPACK #-} !(SmallArray a)

instance Functor FlatBag where
  fmap _ EmptyFlatBag = EmptyFlatBag
  fmap f (UnitFlatBag a) = UnitFlatBag $ f a
  fmap f (TupleFlatBag a b) = TupleFlatBag (f a) (f b)
  fmap f (FlatBag e) = FlatBag $ mapSmallArray f e

instance Foldable FlatBag where
  foldMap _ EmptyFlatBag = mempty
  foldMap f (UnitFlatBag a) = f a
  foldMap f (TupleFlatBag a b) = f a `mappend` f b
  foldMap f (FlatBag arr) = foldMapSmallArray f arr

  length = fromIntegral . sizeFlatBag

instance Traversable FlatBag where
  traverse _ EmptyFlatBag = pure EmptyFlatBag
  traverse f (UnitFlatBag a) = UnitFlatBag <$> f a
  traverse f (TupleFlatBag a b) = TupleFlatBag <$> f a <*> f b
  traverse f fl@(FlatBag arr) = fromList (fromIntegral $ sizeofSmallArray arr) <$> traverse f (elemsFlatBag fl)

instance NFData a => NFData (FlatBag a) where
  rnf EmptyFlatBag = ()
  rnf (UnitFlatBag a) = rnf a
  rnf (TupleFlatBag a b) = rnf a `seq` rnf b
  rnf (FlatBag arr) = rnfSmallArray arr

-- | Create an empty 'FlatBag'.
--
-- The empty 'FlatBag' is shared over all instances.
emptyFlatBag :: FlatBag a
emptyFlatBag = EmptyFlatBag

-- | Create a singleton 'FlatBag'.
unitFlatBag :: a -> FlatBag a
unitFlatBag = UnitFlatBag

-- | Calculate the size of
sizeFlatBag :: FlatBag a -> Word
sizeFlatBag EmptyFlatBag = 0
sizeFlatBag UnitFlatBag{} = 1
sizeFlatBag TupleFlatBag{} = 2
sizeFlatBag (FlatBag arr) = fromIntegral $ sizeofSmallArray arr

-- | Get all elements that are stored in the 'FlatBag'.
elemsFlatBag :: FlatBag a -> [a]
elemsFlatBag EmptyFlatBag = []
elemsFlatBag (UnitFlatBag a) = [a]
elemsFlatBag (TupleFlatBag a b) = [a, b]
elemsFlatBag (FlatBag arr) =
  [indexSmallArray arr i | i <- [0 .. sizeofSmallArray arr - 1]]

-- | Combine two 'FlatBag's.
--
-- The new 'FlatBag' contains all elements from both 'FlatBag's.
--
-- If one of the 'FlatBag's is empty, the old 'FlatBag' is reused.
mappendFlatBag :: FlatBag a -> FlatBag a -> FlatBag a
mappendFlatBag EmptyFlatBag b = b
mappendFlatBag a EmptyFlatBag = a
mappendFlatBag (UnitFlatBag a) (UnitFlatBag b) = TupleFlatBag a b
mappendFlatBag a b =
  fromList (sizeFlatBag a + sizeFlatBag b)
           (elemsFlatBag a ++ elemsFlatBag b)

-- | Store the list in a flattened memory representation, avoiding the memory overhead
-- of a linked list.
--
-- The size 'n' needs to be smaller or equal to the length of the list.
-- If it is smaller than the length of the list, overflowing elements are
-- discarded. It is undefined behaviour to set 'n' to be bigger than the
-- length of the list.
fromList :: Word -> [a] -> FlatBag a
fromList n elts =
  case elts of
    [] -> EmptyFlatBag
    [a] -> UnitFlatBag a
    [a, b] -> TupleFlatBag a b
    xs ->
      FlatBag (listToArray (fromIntegral n) fst snd (zip [0..] xs))

-- | Convert a 'SizedSeq' into its flattened representation.
-- A 'FlatBag a' is more memory efficient than '[a]', if no further modification
-- is necessary.
fromSizedSeq :: SizedSeq a -> FlatBag a
fromSizedSeq s = fromList (sizeSS s) (ssElts s)
