{-# LANGUAGE CPP, MagicHash, UnboxedTuples, UnliftedFFITypes, DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on ByteArrays
--

module Data.Primitive.ByteArray (
  -- * Types
  ByteArray(..), MutableByteArray(..), ByteArray#, MutableByteArray#,

  -- * Allocation
  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,

  -- * Element access
  readByteArray, writeByteArray, indexByteArray,

  -- * Folding
  foldrByteArray,

  -- * Freezing and thawing
  unsafeFreezeByteArray, unsafeThawByteArray,

  -- * Block operations
  copyByteArray, copyMutableByteArray, moveByteArray,
  setByteArray, fillByteArray,

  -- * Information
  sizeofByteArray, sizeofMutableByteArray, sameMutableByteArray,
  byteArrayContents, mutableByteArrayContents
) where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad ( zipWithM_ )
import Data.Primitive.Types

import Foreign.C.Types
import Data.Word ( Word8 )
import GHC.Base ( Int(..) )
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts ( IsList(..) )
#endif
import GHC.Prim
#if __GLASGOW_HASKELL__ >= 706
    hiding (setByteArray#)
#endif

import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )
import Numeric
import System.IO.Unsafe

-- | Byte arrays
data ByteArray = ByteArray ByteArray# deriving ( Typeable )

-- | Mutable byte arrays associated with a primitive state token
data MutableByteArray s = MutableByteArray (MutableByteArray# s)
                                        deriving( Typeable )

-- | Create a new mutable byte array of the specified size in bytes.
newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size in bytes. The garbage
-- collector is guaranteed not to move it.
newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size in bytes and with the
-- give alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedByteArray
  :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
{-# INLINE newAlignedPinnedByteArray #-}
newAlignedPinnedByteArray (I# n#) (I# k#)
  = primitive (\s# -> case newAlignedPinnedByteArray# n# k# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
byteArrayContents :: ByteArray -> Addr
{-# INLINE byteArrayContents #-}
byteArrayContents (ByteArray arr#) = Addr (byteArrayContents# arr#)

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
mutableByteArrayContents :: MutableByteArray s -> Addr
{-# INLINE mutableByteArrayContents #-}
mutableByteArrayContents (MutableByteArray arr#)
  = Addr (byteArrayContents# (unsafeCoerce# arr#))

-- | Check if the two arrays refer to the same memory block.
sameMutableByteArray :: MutableByteArray s -> MutableByteArray s -> Bool
{-# INLINE sameMutableByteArray #-}
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> m ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

-- | Convert an immutable byte array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawByteArray
  :: PrimMonad m => ByteArray -> m (MutableByteArray (PrimState m))
{-# INLINE unsafeThawByteArray #-}
unsafeThawByteArray (ByteArray arr#)
  = primitive (\s# -> (# s#, MutableByteArray (unsafeCoerce# arr#) #))

-- | Size of the byte array in bytes.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

-- | Size of the mutable byte array in bytes.
sizeofMutableByteArray :: MutableByteArray s -> Int
{-# INLINE sizeofMutableByteArray #-}
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
indexByteArray :: Prim a => ByteArray -> Int -> a
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Right-fold over the elements of a 'ByteArray'.
foldrByteArray :: forall a b. (Prim a) => (a -> b -> b) -> b -> ByteArray -> b
foldrByteArray f z arr = go 0
  where
    go i
      | sizeofByteArray arr > i * sz = f (indexByteArray arr i) (go (i+1))
      | otherwise                    = z
    sz = sizeofByteArray arr

fromListN :: Prim a => Int -> [a] -> ByteArray
fromListN n xs = runST $ do
    marr <- newByteArray (n * sizeOf (head xs))
    zipWithM_ (writeByteArray marr) [0..n] xs
    unsafeFreezeByteArray marr

#if __GLASGOW_HASKELL__ >= 702
unI# :: Int -> Int#
unI# (I# n#) = n#
#endif

-- | Copy a slice of an immutable byte array to a mutable byte array.
copyByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> ByteArray           -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) doff (ByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))
#else
  = unsafePrimToPrim
  $ memcpy_ba dst# (fromIntegral doff) src# (fromIntegral soff)
                 (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another array. The two slices
-- may not overlap.
copyMutableByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyMutableByteArray #-}
copyMutableByteArray (MutableByteArray dst#) doff
                     (MutableByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyMutableByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))
#else
  = unsafePrimToPrim
  $ memcpy_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                    (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another, potentially
-- overlapping array.
moveByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE moveByteArray #-}
moveByteArray (MutableByteArray dst#) doff
              (MutableByteArray src#) soff sz
  = unsafePrimToPrim
  $ memmove_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                     (fromIntegral sz)

-- | Fill a slice of a mutable byte array with a value. The offset and length
-- are given in elements of type @a@ rather than in bytes.
setByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -- ^ array to fill
                           -> Int                 -- ^ offset into array
                           -> Int                 -- ^ number of values to fill
                           -> a                   -- ^ value to fill with
                           -> m ()
{-# INLINE setByteArray #-}
setByteArray (MutableByteArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (setByteArray# dst# doff# sz# x)

-- | Fill a slice of a mutable byte array with a byte.
fillByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ array to fill
                 -> Int                 -- ^ offset into array
                 -> Int                 -- ^ number of bytes to fill
                 -> Word8               -- ^ byte to fill with
                 -> m ()
{-# INLINE fillByteArray #-}
fillByteArray = setByteArray

#if __GLASGOW_HASKELL__ < 702
foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_mba :: MutableByteArray# s -> CInt
             -> MutableByteArray# s -> CInt
             -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_ba :: MutableByteArray# s -> CInt
            -> ByteArray# -> CInt
            -> CSize -> IO ()
#endif

foreign import ccall unsafe "primitive-memops.h hsprimitive_memmove"
  memmove_mba :: MutableByteArray# s -> CInt
              -> MutableByteArray# s -> CInt
              -> CSize -> IO ()

instance Data ByteArray where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.ByteArray"

instance Typeable s => Data (MutableByteArray s) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.MutableByteArray"

instance Show ByteArray where
  showsPrec _ ba =
      showString "[" . go 0
    where
      go i
        | i < sizeofByteArray ba = comma . showString "0x" . showHex (indexByteArray ba i :: Word8) . go (i+1)
        | otherwise              = showChar ']'
        where
          comma | i == 0    = id
                | otherwise = showString ", "

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcmp"
  memcmp_ba :: ByteArray# -> ByteArray# -> CSize -> IO CInt

sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
#if __GLASGOW_HASKELL__ >= 708
      r -> isTrue# r
#else
      1# -> True
      0# -> False
#endif

instance Eq ByteArray where
  ba1@(ByteArray ba1#) == ba2@(ByteArray ba2#)
    | sameByteArray ba1# ba2#                    = True
    | sizeofByteArray ba1 /= sizeofByteArray ba2 = False
    | otherwise =
        case unsafeDupablePerformIO $ memcmp_ba ba1# ba2# (fromIntegral $ sizeofByteArray ba1) of
          0 -> True
          _ -> False

instance Ord ByteArray where
  ba1@(ByteArray ba1#) `compare` ba2@(ByteArray ba2#)
    | sameByteArray ba1# ba2# = EQ
    | n1 /= n2                = n1 `compare` n2
    | otherwise =
        case unsafeDupablePerformIO $ memcmp_ba ba1# ba2# (fromIntegral n1) of
          x | x >  0 -> GT
            | x == 0 -> EQ
            | otherwise -> LT
    where
      n1 = sizeofByteArray ba1
      n2 = sizeofByteArray ba2

#if __GLASGOW_HASKELL__ >= 708
instance Exts.IsList ByteArray where
  type Item ByteArray = Word8

  toList = foldrByteArray (:) []
  fromList xs = fromListN (length xs) xs
  fromListN = fromListN
#endif
