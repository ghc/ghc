-- |
-- Module      : Basement.Endianness
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Set endianness tag to a given primitive. This will help for serialising
-- data for protocols (such as the network protocols).
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Basement.Endianness
    (
      ByteSwap
      -- * Big Endian
    , BE(..), toBE, fromBE
      -- * Little Endian
    , LE(..), toLE, fromLE
      -- * System Endianness
    , Endianness(..)
    , endianness
    ) where

import Basement.Compat.Base
import Data.Word (byteSwap16, byteSwap32, byteSwap64)

#if defined(ARCH_IS_LITTLE_ENDIAN) || defined(ARCH_IS_BIG_ENDIAN)
#else
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, peek)
import Data.Word (Word8, Word32)
import System.IO.Unsafe (unsafePerformIO)
#endif

import Data.Bits


-- #if !defined(ARCH_IS_LITTLE_ENDIAN) && !defined(ARCH_IS_BIG_ENDIAN)
-- import Foundation.System.Info (endianness, Endianness(..))
-- #endif

data Endianness =
      LittleEndian
    | BigEndian
    deriving (Eq, Show)

-- | Little Endian value
newtype LE a = LE { unLE :: a }
  deriving (Show, Eq, Typeable, Bits)
instance (ByteSwap a, Ord a) => Ord (LE a) where
    compare e1 e2 = compare (fromLE e1) (fromLE e2)

-- | Big Endian value
newtype BE a = BE { unBE :: a }
  deriving (Show, Eq, Typeable, Bits)
instance (ByteSwap a, Ord a) => Ord (BE a) where
    compare e1 e2 = compare (fromBE e1) (fromBE e2)

-- | Convert a value in cpu endianess to big endian
toBE :: ByteSwap a => a -> BE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toBE = BE . byteSwap
#elif ARCH_IS_BIG_ENDIAN
toBE = BE
#else
toBE = BE . (if endianness == LittleEndian then byteSwap else id)
#endif
{-# INLINE toBE #-}

-- | Convert from a big endian value to the cpu endianness
fromBE :: ByteSwap a => BE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromBE (BE a) = byteSwap a
#elif ARCH_IS_BIG_ENDIAN
fromBE (BE a) = a
#else
fromBE (BE a) = if endianness == LittleEndian then byteSwap a else a
#endif
{-# INLINE fromBE #-}

-- | Convert a value in cpu endianess to little endian
toLE :: ByteSwap a => a -> LE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toLE = LE
#elif ARCH_IS_BIG_ENDIAN
toLE = LE . byteSwap
#else
toLE = LE . (if endianness == LittleEndian then id else byteSwap)
#endif
{-# INLINE toLE #-}

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromLE (LE a) = a
#elif ARCH_IS_BIG_ENDIAN
fromLE (LE a) = byteSwap a
#else
fromLE (LE a) = if endianness == LittleEndian then a else byteSwap a
#endif
{-# INLINE fromLE #-}

-- | endianness of the current architecture
endianness :: Endianness
#ifdef ARCH_IS_LITTLE_ENDIAN
endianness = LittleEndian
#elif ARCH_IS_BIG_ENDIAN
endianness = BigEndian
#else
-- ! ARCH_IS_UNKNOWN_ENDIAN
endianness = unsafePerformIO $ bytesToEndianness <$> word32ToByte input
  where
    input :: Word32
    input = 0x01020304
{-# NOINLINE endianness #-}

word32ToByte :: Word32 -> IO Word8
word32ToByte word = alloca $ \wordPtr -> do
         poke wordPtr word
         peek (castPtr wordPtr)

bytesToEndianness :: Word8 -> Endianness
bytesToEndianness 1 = BigEndian
bytesToEndianness _ = LittleEndian
#endif

-- | Class of types that can be byte-swapped.
--
-- e.g. Word16, Word32, Word64
class ByteSwap a where
    byteSwap :: a -> a
instance ByteSwap Word16 where
    byteSwap = byteSwap16
instance ByteSwap Word32 where
    byteSwap = byteSwap32
instance ByteSwap Word64 where
    byteSwap = byteSwap64
