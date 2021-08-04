-- |
-- Module      : Data.Memory.Endian
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Memory.Endian
    ( Endianness(..)
    , getSystemEndianness
    , BE(..), LE(..)
    , fromBE, toBE
    , fromLE, toLE
    , ByteSwap
    ) where

import Data.Word (Word16, Word32, Word64)
import Foreign.Storable
#if !defined(ARCH_IS_LITTLE_ENDIAN) && !defined(ARCH_IS_BIG_ENDIAN)
import Data.Word (Word8)
import Data.Memory.Internal.Compat (unsafeDoIO)
import Foreign.Marshal.Alloc
import Foreign.Ptr
#endif

import Data.Memory.Internal.Compat (byteSwap64, byteSwap32, byteSwap16)

-- | represent the CPU endianness
--
-- Big endian system stores bytes with the MSB as the first byte.
-- Little endian system stores bytes with the LSB as the first byte.
--
-- middle endian is purposely avoided.
data Endianness = LittleEndian
                | BigEndian
                deriving (Show,Eq)

-- | Return the system endianness
getSystemEndianness :: Endianness
#ifdef ARCH_IS_LITTLE_ENDIAN
getSystemEndianness = LittleEndian
#elif ARCH_IS_BIG_ENDIAN
getSystemEndianness = BigEndian
#else
getSystemEndianness
    | isLittleEndian = LittleEndian
    | isBigEndian    = BigEndian
    | otherwise      = error "cannot determine endianness"
  where
        isLittleEndian = endianCheck == 2
        isBigEndian    = endianCheck == 1
        endianCheck    = unsafeDoIO $ alloca $ \p -> do
                            poke p (0x01000002 :: Word32)
                            peek (castPtr p :: Ptr Word8)
#endif

-- | Little Endian value
newtype LE a = LE { unLE :: a }
    deriving (Show,Eq,Storable)

-- | Big Endian value
newtype BE a = BE { unBE :: a }
    deriving (Show,Eq,Storable)

-- | Convert a value in cpu endianess to big endian
toBE :: ByteSwap a => a -> BE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toBE = BE . byteSwap
#elif ARCH_IS_BIG_ENDIAN
toBE = BE
#else
toBE = BE . (if getSystemEndianness == LittleEndian then byteSwap else id)
#endif
{-# INLINE toBE #-}

-- | Convert from a big endian value to the cpu endianness
fromBE :: ByteSwap a => BE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromBE (BE a) = byteSwap a
#elif ARCH_IS_BIG_ENDIAN
fromBE (BE a) = a
#else
fromBE (BE a) = if getSystemEndianness == LittleEndian then byteSwap a else a
#endif
{-# INLINE fromBE #-}

-- | Convert a value in cpu endianess to little endian
toLE :: ByteSwap a => a -> LE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toLE = LE
#elif ARCH_IS_BIG_ENDIAN
toLE = LE . byteSwap
#else
toLE = LE . (if getSystemEndianness == LittleEndian then id else byteSwap)
#endif
{-# INLINE toLE #-}

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromLE (LE a) = a
#elif ARCH_IS_BIG_ENDIAN
fromLE (LE a) = byteSwap a
#else
fromLE (LE a) = if getSystemEndianness == LittleEndian then a else byteSwap a
#endif
{-# INLINE fromLE #-}

-- | Class of types that can be byte-swapped.
--
-- e.g. Word16, Word32, Word64
class Storable a => ByteSwap a where
    byteSwap :: a -> a
instance ByteSwap Word16 where
    byteSwap = byteSwap16
instance ByteSwap Word32 where
    byteSwap = byteSwap32
instance ByteSwap Word64 where
    byteSwap = byteSwap64
