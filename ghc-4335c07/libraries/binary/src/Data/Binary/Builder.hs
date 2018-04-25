{-# LANGUAGE CPP, MagicHash #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Builder
-- Copyright   : Lennart Kolmodin, Ross Paterson
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Efficient constructions of lazy bytestrings.
--
-- This now re-exports 'Data.ByteString.Lazy.Builder'.
--
-----------------------------------------------------------------------------

module Data.Binary.Builder (
    -- * The Builder type
      Builder
    , toLazyByteString

    -- * Constructing Builders
    , empty
    , singleton
    , append
    , fromByteString        -- :: S.ByteString -> Builder
    , fromLazyByteString    -- :: L.ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,4)
    , fromShortByteString   -- :: T.ByteString -> Builder
#endif
    -- * Flushing the buffer state
    , flush

    -- * Derived Builders
    -- ** Big-endian writes
    , putWord16be           -- :: Word16 -> Builder
    , putWord32be           -- :: Word32 -> Builder
    , putWord64be           -- :: Word64 -> Builder
    , putInt16be            -- :: Int16 -> Builder
    , putInt32be            -- :: Int32 -> Builder
    , putInt64be            -- :: Int64 -> Builder

    -- ** Little-endian writes
    , putWord16le           -- :: Word16 -> Builder
    , putWord32le           -- :: Word32 -> Builder
    , putWord64le           -- :: Word64 -> Builder
    , putInt16le            -- :: Int16 -> Builder
    , putInt32le            -- :: Int32 -> Builder
    , putInt64le            -- :: Int64 -> Builder

    -- ** Host-endian, unaligned writes
    , putWordhost           -- :: Word -> Builder
    , putWord16host         -- :: Word16 -> Builder
    , putWord32host         -- :: Word32 -> Builder
    , putWord64host         -- :: Word64 -> Builder
    , putInthost            -- :: Int -> Builder
    , putInt16host          -- :: Int16 -> Builder
    , putInt32host          -- :: Int32 -> Builder
    , putInt64host          -- :: Int64 -> Builder

      -- ** Unicode
    , putCharUtf8
    , putStringUtf8
    ) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as T
#endif

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as Prim
import Data.ByteString.Builder ( Builder, toLazyByteString )
import Data.ByteString.Builder.Extra ( flush )
import Data.Monoid
import Data.Word
import Data.Int
import Prelude -- Silence AMP warning.

------------------------------------------------------------------------

-- | /O(1)./ The empty Builder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: Builder
empty = mempty
{-# INLINE empty #-}

-- | /O(1)./ A Builder taking a single byte, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Word8 -> Builder
singleton = B.word8
{-# INLINE singleton #-}

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two Builders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: Builder -> Builder -> Builder
append = mappend
{-# INLINE append #-}

-- | /O(1)./ A Builder taking a 'S.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromByteString' bs) = 'L.fromChunks' [bs]@
--
fromByteString :: S.ByteString -> Builder
fromByteString = B.byteString
{-# INLINE fromByteString #-}

-- | /O(1)./ A Builder taking a lazy 'L.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromLazyByteString' bs) = bs@
--
fromLazyByteString :: L.ByteString -> Builder
fromLazyByteString = B.lazyByteString
{-# INLINE fromLazyByteString #-}

#if MIN_VERSION_bytestring(0,10,4)
-- | /O(n)./ A builder taking 'T.ShortByteString' and copy it to a Builder,
-- satisfying
--
-- * @'toLazyByteString' ('fromShortByteString' bs) = 'L.fromChunks' ['T.fromShort' bs]
fromShortByteString :: T.ShortByteString -> Builder
fromShortByteString = B.shortByteString
{-# INLINE fromShortByteString #-}
#endif

------------------------------------------------------------------------

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> Builder
putWord16be = B.word16BE
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> Builder
putWord16le = B.word16LE
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Builder
putWord32be = B.word32BE
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> Builder
putWord32le = B.word32LE
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Builder
putWord64be = B.word64BE
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> Builder
putWord64le = B.word64LE
{-# INLINE putWord64le #-}

-- | Write a Int16 in big endian format
putInt16be :: Int16 -> Builder
putInt16be = B.int16BE
{-# INLINE putInt16be #-}

-- | Write a Int16 in little endian format
putInt16le :: Int16 -> Builder
putInt16le = B.int16LE
{-# INLINE putInt16le #-}

-- | Write a Int32 in big endian format
putInt32be :: Int32 -> Builder
putInt32be = B.int32BE
{-# INLINE putInt32be #-}

-- | Write a Int32 in little endian format
putInt32le :: Int32 -> Builder
putInt32le = B.int32LE
{-# INLINE putInt32le #-}

-- | Write a Int64 in big endian format
putInt64be :: Int64 -> Builder
putInt64be = B.int64BE

-- | Write a Int64 in little endian format
putInt64le :: Int64 -> Builder
putInt64le = B.int64LE


------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost :: Word -> Builder
putWordhost = Prim.primFixed Prim.wordHost
{-# INLINE putWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
putWord16host :: Word16 -> Builder
putWord16host = Prim.primFixed Prim.word16Host
{-# INLINE putWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putWord32host :: Word32 -> Builder
putWord32host = Prim.primFixed Prim.word32Host
{-# INLINE putWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
putWord64host :: Word64 -> Builder
putWord64host = Prim.primFixed Prim.word64Host
{-# INLINE putWord64host #-}

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Int is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putInthost :: Int -> Builder
putInthost = Prim.primFixed Prim.intHost
{-# INLINE putInthost #-}

-- | Write a Int16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
putInt16host :: Int16 -> Builder
putInt16host = Prim.primFixed Prim.int16Host
{-# INLINE putInt16host #-}

-- | Write a Int32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putInt32host :: Int32 -> Builder
putInt32host = Prim.primFixed Prim.int32Host
{-# INLINE putInt32host #-}

-- | Write a Int64 in native host order.
-- On a 32 bit machine we write two host order Int32s, in big endian form.
-- 8 bytes will be written, unaligned.
putInt64host :: Int64 -> Builder
putInt64host = Prim.primFixed Prim.int64Host
{-# INLINE putInt64host #-}


------------------------------------------------------------------------
-- Unicode

-- | Write a character using UTF-8 encoding.
putCharUtf8 :: Char -> Builder
putCharUtf8 = Prim.primBounded Prim.charUtf8
{-# INLINE putCharUtf8 #-}

-- | Write a String using UTF-8 encoding.
putStringUtf8 :: String -> Builder
putStringUtf8 = Prim.primMapListBounded Prim.charUtf8
{-# INLINE putStringUtf8 #-}
