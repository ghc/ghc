{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

#if MIN_VERSION_base(4,9,0)
#define HAS_SEMIGROUP
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : stable
-- Portability : Portable to Hugs and GHC. Requires MPTCs
--
-- The Put monad. A monad for efficiently constructing lazy bytestrings.
--
-----------------------------------------------------------------------------

module Data.Binary.Put (

    -- * The Put type
      Put
    , PutM(..)
    , runPut
    , runPutM
    , putBuilder
    , execPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putInt8
    , putByteString
    , putLazyByteString
#if MIN_VERSION_bytestring(0,10,4)
    , putShortByteString
#endif

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be
    , putInt16be
    , putInt32be
    , putInt64be
    , putFloatbe
    , putDoublebe

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le
    , putInt16le
    , putInt32le
    , putInt64le
    , putFloatle
    , putDoublele

    -- * Host-endian, unaligned writes
    , putWordhost           -- :: Word   -> Put
    , putWord16host         -- :: Word16 -> Put
    , putWord32host         -- :: Word32 -> Put
    , putWord64host         -- :: Word64 -> Put
    , putInthost            -- :: Int    -> Put
    , putInt16host          -- :: Int16  -> Put
    , putInt32host          -- :: Int32  -> Put
    , putInt64host          -- :: Int64  -> Put
    , putFloathost
    , putDoublehost

    -- * Unicode
    , putCharUtf8
    , putStringUtf8

  ) where

import qualified Data.Monoid as Monoid
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.Binary.Builder as B

import Data.Int
import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
#if MIN_VERSION_bytestring(0,10,4)
import Data.ByteString.Short
#endif

#ifdef HAS_SEMIGROUP
import Data.Semigroup
#endif

import Control.Applicative
import Prelude -- Silence AMP warning.

-- needed for casting Floats/Doubles to words.
import Data.Binary.FloatCast (floatToWord, doubleToWord)

------------------------------------------------------------------------

-- XXX Strict in buffer only.
data PairS a = PairS a !Builder

sndS :: PairS a -> Builder
sndS (PairS _ b) = b

-- | The PutM type. A Writer monad over the efficient Builder monoid.
newtype PutM a = Put { unPut :: PairS a }

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()

instance Functor PutM where
        fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w
        {-# INLINE fmap #-}

instance Applicative PutM where
        pure a  = Put $ PairS a Monoid.mempty
        {-# INLINE pure #-}

        m <*> k = Put $
            let PairS f w  = unPut m
                PairS x w' = unPut k
            in PairS (f x) (w `Monoid.mappend` w')

        m *> k  = Put $
            let PairS _ w  = unPut m
                PairS b w' = unPut k
            in PairS b (w `Monoid.mappend` w')
        {-# INLINE (*>) #-}

-- Standard Writer monad, with aggressive inlining
instance Monad PutM where
    m >>= k  = Put $
        let PairS a w  = unPut m
            PairS b w' = unPut (k a)
        in PairS b (w `Monoid.mappend` w')
    {-# INLINE (>>=) #-}

    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

instance Monoid.Monoid (PutM ()) where
    mempty = pure ()
    {-# INLINE mempty #-}

#ifdef HAS_SEMIGROUP
    mappend = (<>)
#else
    mappend = mappend'
#endif
    {-# INLINE mappend #-}

mappend' :: Put -> Put -> Put
mappend' m k = Put $
    let PairS _ w  = unPut m
        PairS _ w' = unPut k
    in PairS () (w `Monoid.mappend` w')
{-# INLINE mappend' #-}

#ifdef HAS_SEMIGROUP
instance Semigroup (PutM ()) where
    (<>) = mappend'
    {-# INLINE (<>) #-}
#endif

tell :: Builder -> Put
tell b = Put $ PairS () b
{-# INLINE tell #-}

putBuilder :: Builder -> Put
putBuilder = tell
{-# INLINE putBuilder #-}

-- | Run the 'Put' monad
execPut :: PutM a -> Builder
execPut = sndS . unPut
{-# INLINE execPut #-}

-- | Run the 'Put' monad with a serialiser
runPut :: Put -> L.ByteString
runPut = toLazyByteString . sndS . unPut
{-# INLINE runPut #-}

-- | Run the 'Put' monad with a serialiser and get its result
runPutM :: PutM a -> (a, L.ByteString)
runPutM (Put (PairS f s)) = (f, toLazyByteString s)
{-# INLINE runPutM #-}

------------------------------------------------------------------------

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = tell B.flush
{-# INLINE flush #-}

-- | Efficiently write a byte into the output buffer
putWord8            :: Word8 -> Put
putWord8            = tell . B.singleton
{-# INLINE putWord8 #-}

-- | Efficiently write a signed byte into the output buffer
putInt8            :: Int8 -> Put
putInt8            = tell . B.singleton . fromIntegral
{-# INLINE putInt8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: S.ByteString -> Put
putByteString       = tell . B.fromByteString
{-# INLINE putByteString #-}

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: L.ByteString -> Put
putLazyByteString   = tell . B.fromLazyByteString
{-# INLINE putLazyByteString #-}

#if MIN_VERSION_bytestring(0,10,4)
-- | Write 'ShortByteString' to the buffer
putShortByteString :: ShortByteString -> Put
putShortByteString = tell . B.fromShortByteString
{-# INLINE putShortByteString #-}
#endif

-- | Write a Word16 in big endian format
putWord16be         :: Word16 -> Put
putWord16be         = tell . B.putWord16be
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le         :: Word16 -> Put
putWord16le         = tell . B.putWord16le
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be         :: Word32 -> Put
putWord32be         = tell . B.putWord32be
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le         :: Word32 -> Put
putWord32le         = tell . B.putWord32le
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be         :: Word64 -> Put
putWord64be         = tell . B.putWord64be
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le         :: Word64 -> Put
putWord64le         = tell . B.putWord64le
{-# INLINE putWord64le #-}

-- | Write an Int16 in big endian format
putInt16be         :: Int16 -> Put
putInt16be         = tell . B.putInt16be
{-# INLINE putInt16be #-}

-- | Write an Int16 in little endian format
putInt16le         :: Int16 -> Put
putInt16le         = tell . B.putInt16le
{-# INLINE putInt16le #-}

-- | Write an Int32 in big endian format
putInt32be         :: Int32 -> Put
putInt32be         = tell . B.putInt32be
{-# INLINE putInt32be #-}

-- | Write an Int32 in little endian format
putInt32le         :: Int32 -> Put
putInt32le         = tell . B.putInt32le
{-# INLINE putInt32le #-}

-- | Write an Int64 in big endian format
putInt64be         :: Int64 -> Put
putInt64be         = tell . B.putInt64be
{-# INLINE putInt64be #-}

-- | Write an Int64 in little endian format
putInt64le         :: Int64 -> Put
putInt64le         = tell . B.putInt64le
{-# INLINE putInt64le #-}


------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost         :: Word -> Put
putWordhost         = tell . B.putWordhost
{-# INLINE putWordhost #-}

-- | /O(1)./ Write a Word16 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord16host       :: Word16 -> Put
putWord16host       = tell . B.putWord16host
{-# INLINE putWord16host #-}

-- | /O(1)./ Write a Word32 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord32host       :: Word32 -> Put
putWord32host       = tell . B.putWord32host
{-# INLINE putWord32host #-}

-- | /O(1)./ Write a Word64 in native host order
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- For portability issues see @putWordhost@.
putWord64host       :: Word64 -> Put
putWord64host       = tell . B.putWord64host
{-# INLINE putWord64host #-}

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Int is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putInthost         :: Int -> Put
putInthost         = tell . B.putInthost
{-# INLINE putInthost #-}

-- | /O(1)./ Write an Int16 in native host order and host endianness.
-- For portability issues see @putInthost@.
putInt16host       :: Int16 -> Put
putInt16host       = tell . B.putInt16host
{-# INLINE putInt16host #-}

-- | /O(1)./ Write an Int32 in native host order and host endianness.
-- For portability issues see @putInthost@.
putInt32host       :: Int32 -> Put
putInt32host       = tell . B.putInt32host
{-# INLINE putInt32host #-}

-- | /O(1)./ Write an Int64 in native host order
-- On a 32 bit machine we write two host order Int32s, in big endian form.
-- For portability issues see @putInthost@.
putInt64host       :: Int64 -> Put
putInt64host       = tell . B.putInt64host
{-# INLINE putInt64host #-}

------------------------------------------------------------------------
-- Floats/Doubles

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Float -> Put
putFloatbe = putWord32be . floatToWord
{-# INLINE putFloatbe #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: Float -> Put
putFloatle = putWord32le . floatToWord
{-# INLINE putFloatle #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: Float -> Put
putFloathost = putWord32host . floatToWord
{-# INLINE putFloathost #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Double -> Put
putDoublebe = putWord64be . doubleToWord
{-# INLINE putDoublebe #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: Double -> Put
putDoublele = putWord64le . doubleToWord
{-# INLINE putDoublele #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: Double -> Put
putDoublehost = putWord64host . doubleToWord
{-# INLINE putDoublehost #-}

------------------------------------------------------------------------
-- Unicode

-- | Write a character using UTF-8 encoding.
putCharUtf8 :: Char -> Put
putCharUtf8 = tell . B.putCharUtf8
{-# INLINE putCharUtf8 #-}

-- | Write a String using UTF-8 encoding.
putStringUtf8 :: String -> Put
putStringUtf8 = tell . B.putStringUtf8
{-# INLINE putStringUtf8 #-}
