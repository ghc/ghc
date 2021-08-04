{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Primitive.Ptr
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on machine addresses
--
-- @since 0.6.4.0

module Data.Primitive.Ptr (
  -- * Types
  Ptr(..),

  -- * Address arithmetic
  nullPtr, advancePtr, subtractPtr,

  -- * Element access
  indexOffPtr, readOffPtr, writeOffPtr,

  -- * Block operations
  copyPtr, movePtr, setPtr

#if __GLASGOW_HASKELL__ >= 708
  , copyPtrToMutablePrimArray
  , copyPtrToMutableByteArray
#endif
) where

import Control.Monad.Primitive
import Data.Primitive.Types
#if __GLASGOW_HASKELL__ >= 708
import Data.Primitive.PrimArray (MutablePrimArray(..))
import Data.Primitive.ByteArray (MutableByteArray(..))
#endif

import GHC.Base ( Int(..) )
import GHC.Exts

import GHC.Ptr
import Foreign.Marshal.Utils


-- | Offset a pointer by the given number of elements.
advancePtr :: forall a. Prim a => Ptr a -> Int -> Ptr a
{-# INLINE advancePtr #-}
advancePtr (Ptr a#) (I# i#) = Ptr (plusAddr# a# (i# *# sizeOf# (undefined :: a)))

-- | Subtract a pointer from another pointer. The result represents
--   the number of elements of type @a@ that fit in the contiguous
--   memory range bounded by these two pointers.
subtractPtr :: forall a. Prim a => Ptr a -> Ptr a -> Int
{-# INLINE subtractPtr #-}
subtractPtr (Ptr a#) (Ptr b#) = I# (quotInt# (minusAddr# a# b#) (sizeOf# (undefined :: a)))

-- | Read a value from a memory position given by a pointer and an offset.
-- The memory block the address refers to must be immutable. The offset is in
-- elements of type @a@ rather than in bytes.
indexOffPtr :: Prim a => Ptr a -> Int -> a
{-# INLINE indexOffPtr #-}
indexOffPtr (Ptr addr#) (I# i#) = indexOffAddr# addr# i#

-- | Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
readOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> m a
{-# INLINE readOffPtr #-}
readOffPtr (Ptr addr#) (I# i#) = primitive (readOffAddr# addr# i#)

-- | Write a value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
writeOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> a -> m ()
{-# INLINE writeOffPtr #-}
writeOffPtr (Ptr addr#) (I# i#) x = primitive_ (writeOffAddr# addr# i# x)

-- | Copy the given number of elements from the second 'Ptr' to the first. The
-- areas may not overlap.
copyPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> Ptr a -- ^ source pointer
  -> Int -- ^ number of elements
  -> m ()
{-# INLINE copyPtr #-}
copyPtr (Ptr dst#) (Ptr src#) n
  = unsafePrimToPrim $ copyBytes (Ptr dst#) (Ptr src#) (n * sizeOf (undefined :: a))

-- | Copy the given number of elements from the second 'Ptr' to the first. The
-- areas may overlap.
movePtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination address
  -> Ptr a -- ^ source address
  -> Int -- ^ number of elements
  -> m ()
{-# INLINE movePtr #-}
movePtr (Ptr dst#) (Ptr src#) n
  = unsafePrimToPrim $ moveBytes (Ptr dst#) (Ptr src#) (n * sizeOf (undefined :: a))

-- | Fill a memory block with the given value. The length is in
-- elements of type @a@ rather than in bytes.
setPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> a -> m ()
{-# INLINE setPtr #-}
setPtr (Ptr addr#) (I# n#) x = primitive_ (setOffAddr# addr# 0# n# x)


#if __GLASGOW_HASKELL__ >= 708
-- | Copy from a pointer to a mutable primitive array.
-- The offset and length are given in elements of type @a@.
-- This function is only available when building with GHC 7.8
-- or newer.
copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ destination offset
  -> Ptr a -- ^ source pointer
  -> Int -- ^ number of elements
  -> m ()
{-# INLINE copyPtrToMutablePrimArray #-}
copyPtrToMutablePrimArray (MutablePrimArray ba#) (I# doff#) (Ptr addr#) (I# n#) =
  primitive_ (copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#))
  where
  siz# = sizeOf# (undefined :: a)

-- | Copy from a pointer to a mutable byte array.
-- The offset and length are given in elements of type @a@.
-- This function is only available when building with GHC 7.8
-- or newer.
copyPtrToMutableByteArray :: forall m a. (PrimMonad m, Prim a)
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int   -- ^ destination offset given in elements of type @a@
  -> Ptr a -- ^ source pointer
  -> Int   -- ^ number of elements
  -> m ()
{-# INLINE copyPtrToMutableByteArray #-}
copyPtrToMutableByteArray (MutableByteArray ba#) (I# doff#) (Ptr addr#) (I# n#) =
  primitive_ (copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#))
  where
  siz# = sizeOf# (undefined :: a)
#endif
