{-# Language MagicHash #-}
{-# Language UnboxedTuples #-}
{-# Language DeriveDataTypeable #-}

-- |
-- Module      : Data.Primitive.UnliftedArray
-- Copyright   : (c) Dan Doel 2016
-- License     : BSD-style
--
-- Maintainer  : Libraries <libraries@haskell.org>
-- Portability : non-portable
--
-- GHC contains three general classes of value types:
--
--   1. Unboxed types: values are machine values made up of fixed numbers of bytes
--   2. Unlifted types: values are pointers, but strictly evaluated
--   3. Lifted types: values are pointers, lazily evaluated
--
-- The first category can be stored in a 'ByteArray', and this allows types in
-- category 3 that are simple wrappers around category 1 types to be stored
-- more efficiently using a 'ByteArray'. This module provides the same facility
-- for category 2 types.
--
-- GHC has two primitive types, 'ArrayArray#' and 'MutableArrayArray#'. These
-- are arrays of pointers, but of category 2 values, so they are known to not
-- be bottom. This allows types that are wrappers around such types to be stored
-- in an array without an extra level of indirection.
--
-- The way that the 'ArrayArray#' API works is that one can read and write
-- 'ArrayArray#' values to the positions. This works because all category 2
-- types share a uniform representation, unlike unboxed values which are
-- represented by varying (by type) numbers of bytes. However, using the
-- this makes the internal API very unsafe to use, as one has to coerce values
-- to and from 'ArrayArray#'.
--
-- The API presented by this module is more type safe. 'UnliftedArray' and
-- 'MutableUnliftedArray' are parameterized by the type of arrays they contain, and
-- the coercions necessary are abstracted into a class, 'PrimUnlifted', of things
-- that are eligible to be stored.

module Data.Primitive.UnliftedArray
  ( UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted(..)
  , unsafeNewUnliftedArray
  , newUnliftedArray
  , setUnliftedArray
  , sizeofUnliftedArray
  , sizeofMutableUnliftedArray
  , readUnliftedArray
  , writeUnliftedArray
  , indexUnliftedArray
  , indexUnliftedArrayM
  , unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , sameMutableUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , cloneUnliftedArray
  , cloneMutableUnliftedArray
-- Missing operations:
--  , unsafeThawUnliftedArray
  ) where

import Data.Typeable

import GHC.Prim
import GHC.Base (Int(..))

import Control.Monad.Primitive

import Control.Monad.ST (runST)

import Data.Primitive.Internal.Compat ( isTrue# )

import           Data.Primitive.Array (Array)
import qualified Data.Primitive.Array as A
import           Data.Primitive.ByteArray (ByteArray)
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Primitive.MutVar as MV

-- | Immutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data UnliftedArray e = UnliftedArray ArrayArray#
  deriving (Typeable)

-- | Mutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data MutableUnliftedArray s e = MutableUnliftedArray (MutableArrayArray# s)
  deriving (Typeable)

-- | Classifies the types that are able to be stored in 'UnliftedArray' and
-- 'MutableUnliftedArray'. These should be types that are just liftings of the
-- unlifted pointer types, so that their internal contents can be safely coerced
-- into an 'ArrayArray#'.
class PrimUnlifted a where
  toArrayArray# :: a -> ArrayArray#
  fromArrayArray# :: ArrayArray# -> a

instance PrimUnlifted (UnliftedArray e) where
  toArrayArray# (UnliftedArray aa#) = aa#
  fromArrayArray# aa# = UnliftedArray aa#

instance PrimUnlifted (MutableUnliftedArray s e) where
  toArrayArray# (MutableUnliftedArray maa#) = unsafeCoerce# maa#
  fromArrayArray# aa# = MutableUnliftedArray (unsafeCoerce# aa#)

instance PrimUnlifted (Array a) where
  toArrayArray# (A.Array a#) = unsafeCoerce# a#
  fromArrayArray# aa# = A.Array (unsafeCoerce# aa#)

instance PrimUnlifted (A.MutableArray s a) where
  toArrayArray# (A.MutableArray ma#) = unsafeCoerce# ma#
  fromArrayArray# aa# = A.MutableArray (unsafeCoerce# aa#)

instance PrimUnlifted ByteArray where
  toArrayArray# (BA.ByteArray ba#) = unsafeCoerce# ba#
  fromArrayArray# aa# = BA.ByteArray (unsafeCoerce# aa#)

instance PrimUnlifted (BA.MutableByteArray s) where
  toArrayArray# (BA.MutableByteArray mba#) = unsafeCoerce# mba#
  fromArrayArray# aa# = BA.MutableByteArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallArray a) where
  toArrayArray# (SA.SmallArray sa#) = unsafeCoerce# sa#
  fromArrayArray# aa# = SA.SmallArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallMutableArray s a) where
  toArrayArray# (SA.SmallMutableArray sma#) = unsafeCoerce# sma#
  fromArrayArray# aa# = SA.SmallMutableArray (unsafeCoerce# aa#)

instance PrimUnlifted (MV.MutVar s a) where
  toArrayArray# (MV.MutVar mv#) = unsafeCoerce# mv#
  fromArrayArray# aa# = MV.MutVar (unsafeCoerce# aa#)

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe, because it
-- allows access to the raw contents of the underlying 'ArrayArray#'.
unsafeNewUnliftedArray
  :: (PrimMonad m)
  => Int -- ^ size
  -> m (MutableUnliftedArray (PrimState m) a)
unsafeNewUnliftedArray (I# i#) = primitive $ \s -> case newArrayArray# i# s of
  (# s', maa# #) -> (# s', MutableUnliftedArray maa# #)
{-# inline unsafeNewUnliftedArray #-}

-- | Sets all the positions in an unlifted array to the designated value.
setUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> m ()
setUnliftedArray mua v = loop $ sizeofMutableUnliftedArray mua - 1
 where
 loop i | i < 0     = return ()
        | otherwise = writeUnliftedArray mua i v >> loop (i-1)
{-# inline setUnliftedArray #-}

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents. This is slower than 'unsafeNewUnliftedArray', but safer.
newUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray len v =
  unsafeNewUnliftedArray len >>= \mua -> setUnliftedArray mua v >> return mua
{-# inline newUnliftedArray #-}

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
sizeofUnliftedArray (UnliftedArray aa#) = I# (sizeofArrayArray# aa#)
{-# inline sizeofUnliftedArray #-}

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
  = I# (sizeofMutableArrayArray# maa#)
{-# inline sizeofMutableUnliftedArray #-}

-- Internal indexing function.
--
-- Note: ArrayArray# is strictly evaluated, so this should have similar
-- consequences to indexArray#, where matching on the unboxed single causes the
-- array access to happen.
indexUnliftedArrayU
  :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> (# a #)
indexUnliftedArrayU (UnliftedArray src#) (I# i#)
  = case indexArrayArrayArray# src# i# of
      aa# -> (# fromArrayArray# aa# #)
{-# inline indexUnliftedArrayU #-}

-- | Gets the value at the specified position of an 'UnliftedArray'.
indexUnliftedArray
  :: PrimUnlifted a
  => UnliftedArray a -- ^ source
  -> Int -- ^ index
  -> a
indexUnliftedArray ua i
  = case indexUnliftedArrayU ua i of (# v #) -> v
{-# inline indexUnliftedArray #-}

-- | Gets the value at the specified position of an 'UnliftedArray'.
-- The purpose of the 'Monad' is to allow for being eager in the
-- 'UnliftedArray' value without having to introduce a data dependency
-- directly on the result value.
--
-- It should be noted that this is not as much of a problem as with a normal
-- 'Array', because elements of an 'UnliftedArray' are guaranteed to not
-- be exceptional. This function is provided in case it is more desirable
-- than being strict in the result value.
indexUnliftedArrayM
  :: (PrimUnlifted a, Monad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ index
  -> m a
indexUnliftedArrayM ua i
  = case indexUnliftedArrayU ua i of
      (# v #) -> return v
{-# inline indexUnliftedArrayM #-}

-- | Gets the value at the specified position of a 'MutableUnliftedArray'.
readUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ index
  -> m a
readUnliftedArray (MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case readArrayArrayArray# maa# i# s of
      (# s', aa# #) -> (# s',  fromArrayArray# aa# #)
{-# inline readUnliftedArray #-}

-- | Sets the value at the specified position of a 'MutableUnliftedArray'.
writeUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ index
  -> a -- ^ value
  -> m ()
writeUnliftedArray (MutableUnliftedArray maa#) (I# i#) a
  = primitive_ (writeArrayArrayArray# maa# i# (toArrayArray# a))
{-# inline writeUnliftedArray #-}

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a
  -> m (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
  = primitive $ \s -> case unsafeFreezeArrayArray# maa# s of
      (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
  :: MutableUnliftedArray s a
  -> MutableUnliftedArray s a
  -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
  = isTrue# (sameMutableArrayArray# maa1# maa2#)
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
copyUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (UnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyArrayArray# src soff dst doff ln
{-# inline copyUnliftedArray #-}

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutableUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (MutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableArrayArray# src soff dst doff ln
{-# inline copyMutableUnliftedArray #-}

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (UnliftedArray a)
freezeUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyMutableUnliftedArray dst 0 src off len
  unsafeFreezeUnliftedArray dst
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: (PrimMonad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
thawUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyUnliftedArray dst 0 src off len
  return dst
{-# inline thawUnliftedArray #-}

-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> UnliftedArray a
cloneUnliftedArray src off len =
  runST $ thawUnliftedArray src off len >>= unsafeFreezeUnliftedArray
{-# inline cloneUnliftedArray #-}

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
cloneMutableUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyMutableUnliftedArray dst 0 src off len
  return dst
{-# inline cloneMutableUnliftedArray #-}

instance Eq (MutableUnliftedArray s a) where
  (==) = sameMutableUnliftedArray

instance (Eq a, PrimUnlifted a) => Eq (UnliftedArray a) where
  aa1 == aa2 = sizeofUnliftedArray aa1 == sizeofUnliftedArray aa2
            && loop (sizeofUnliftedArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexUnliftedArray aa1 i == indexUnliftedArray aa2 i && loop (i-1)
