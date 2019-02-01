{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Primitive.PrimArray
  ( PrimArray(..)
  , MutablePrimArray(..)
  , newPrimArray
  , newPinnedPrimArray
  , writePrimArray
  , readPrimArray
  , copyMutablePrimArray
  , mutablePrimArrayContents
  , getSizeofMutablePrimArray
  , touchMutablePrimArray
  ) where

import GHC.Base
import GHC.Primitive.Monad
import GHC.Ptr (Ptr(..))

-- | Arrays of unboxed elements. This accepts types like 'Double', 'Char',
-- 'Int', and 'Word', as well as their fixed-length variants ('Word8',
-- 'Word16', etc.). Since the elements are unboxed, a 'PrimArray' is strict
-- in its elements. This differs from the behavior of 'Array', which is lazy
-- in its elements.
data PrimArray a = PrimArray ByteArray#

-- | Mutable primitive arrays associated with a primitive state token.
-- These can be written to and read from in a monadic context that supports
-- sequencing such as 'IO' or 'ST'. Typically, a mutable primitive array will
-- be built and then convert to an immutable primitive array using
-- 'unsafeFreezePrimArray'. However, it is also acceptable to simply discard
-- a mutable primitive array since it lives in managed memory and will be
-- garbage collected when no longer referenced.
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

-- | Create a new mutable primitive array of the given length. The
-- underlying memory is left uninitialized.
newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

-- | Write an element to the given index.
writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)


readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)


-- | Create a /pinned/ primitive array of the specified size in elements. The garbage
-- collector is guaranteed not to move it.
newPinnedPrimArray :: forall m a. (PrimMonad m, Prim a)
  => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# (n# *# sizeOf# (undefined :: a)) s# of
                        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #))

-- | Copy part of a mutable array into another mutable array.
--   In the case that the destination and
--   source arrays are the same, the regions may overlap.
copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
mutablePrimArrayContents :: MutablePrimArray s a -> Ptr a
{-# INLINE mutablePrimArrayContents #-}
mutablePrimArrayContents (MutablePrimArray arr#)
  = Ptr (byteArrayContents# (unsafeCoerce# arr#))


-- | Get the size of a mutable primitive array in elements. Unlike 'sizeofMutablePrimArray',
-- this function ensures sequencing in the presence of resizing.
getSizeofMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> m Int
{-# INLINE getSizeofMutablePrimArray #-}
getSizeofMutablePrimArray (MutablePrimArray arr#)
  = primitive (\s# ->
      case getSizeofMutableByteArray# arr# s# of
        (# s'#, sz# #) -> (# s'#, I# (quotInt# sz# (sizeOf# (undefined :: a))) #)
    )

touchMutablePrimArray :: forall a.
     MutablePrimArray RealWorld a
  -> IO ()
touchMutablePrimArray (MutablePrimArray arr#)
  = primitive_ (\s# -> touch# arr# s#)
