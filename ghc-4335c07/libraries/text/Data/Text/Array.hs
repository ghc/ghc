{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualified
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(Array, aBA)
    , MArray(MArray, maBA)

    -- * Functions
    , copyM
    , copyI
    , empty
    , equal
#if defined(ASSERTS)
    , length
#endif
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , unsafeWrite
    ) where

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.Text.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

#include "MachDeps.h"

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
#if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Data.Bits ((.&.), xor)
import Data.Text.Internal.Unsafe (inlinePerformIO)
import Data.Text.Internal.Unsafe.Shift (shiftL, shiftR)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(CInt), CSize(CSize))
#else
import Foreign.C.Types (CInt, CSize)
#endif
import GHC.Base (ByteArray#, MutableByteArray#, Int(..),
                 indexWord16Array#, newByteArray#,
                 unsafeFreezeByteArray#, writeWord16Array#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word16(..))
import Prelude hiding (length, read)

-- | Immutable array type.
data Array = Array {
      aBA :: ByteArray#
#if defined(ASSERTS)
    , aLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

-- | Mutable array type, for use in the ST monad.
data MArray s = MArray {
      maBA :: MutableByteArray# s
#if defined(ASSERTS)
    , maLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

#if defined(ASSERTS)
-- | Operations supported by all arrays.
class IArray a where
    -- | Return the length of an array.
    length :: a -> Int

instance IArray Array where
    length = aLen
    {-# INLINE length #-}

instance IArray (MArray s) where
    length = maLen
    {-# INLINE length #-}
#endif

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new n
  | n < 0 || n .&. highBit /= 0 = array_size_error
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr#
#if defined(ASSERTS)
                                n
#endif
                                #)
  where !(I# len#) = bytesInArray n
        highBit    = maxBound `xor` (maxBound `shiftR` 1)
{-# INLINE new #-}

array_size_error :: a
array_size_error = error "Data.Text.Array.new: size overflow"

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s1# ->
    case unsafeFreezeByteArray# maBA s1# of
        (# s2#, ba# #) -> (# s2#, Array ba#
#if defined(ASSERTS)
                             maLen
#endif
                             #)
{-# INLINE unsafeFreeze #-}

-- | Indicate how many bytes would be used for an array of the given
-- size.
bytesInArray :: Int -> Int
bytesInArray n = n `shiftL` 1
{-# INLINE bytesInArray #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex :: Array -> Int -> Word16
unsafeIndex Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex",aLen,i)
    case indexWord16Array# aBA i# of r# -> (W16# r#)
{-# INLINE unsafeIndex #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite :: MArray s -> Int -> Word16 -> ST s ()
unsafeWrite MArray{..} i@(I# i#) (W16# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite",maLen,i)
  case writeWord16Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word16]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))
{-# INLINE run2 #-}

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dest didx src sidx count
    | count <= 0 = return ()
    | otherwise =
#if defined(ASSERTS)
    assert (sidx + count <= length src) .
    assert (didx + count <= length dest) .
#endif
    unsafeIOToST $ memcpyM (maBA dest) (fromIntegral didx)
                           (maBA src) (fromIntegral sidx)
                           (fromIntegral count)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ First offset in destination /not/ to
                                -- copy (i.e. /not/ length)
      -> ST s ()
copyI dest i0 src j0 top
    | i0 >= top = return ()
    | otherwise = unsafeIOToST $
                  memcpyI (maBA dest) (fromIntegral i0)
                          (aBA src) (fromIntegral j0)
                          (fromIntegral (top-i0))
{-# INLINE copyI #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal arrA offA arrB offB count = inlinePerformIO $ do
  i <- memcmp (aBA arrA) (fromIntegral offA)
                     (aBA arrB) (fromIntegral offB) (fromIntegral count)
  return $! i == 0
{-# INLINE equal #-}

foreign import ccall unsafe "_hs_text_memcpy" memcpyI
    :: MutableByteArray# s -> CSize -> ByteArray# -> CSize -> CSize -> IO ()

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "_hs_text_memcpy" memcpyM
    :: MutableByteArray# s -> CSize -> MutableByteArray# s -> CSize -> CSize
    -> IO ()
