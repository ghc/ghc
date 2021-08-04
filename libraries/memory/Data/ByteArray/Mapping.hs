-- |
-- Module      : Data.ByteArray.Mapping
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
module Data.ByteArray.Mapping
    ( toW64BE
    , toW64LE
    , fromW64BE
    , mapAsWord64
    , mapAsWord128
    ) where

import           Data.ByteArray.Types
import           Data.ByteArray.Methods
import           Data.Memory.Internal.Compat
import           Data.Memory.Internal.Imports hiding (empty)
import           Data.Memory.Endian
import           Data.Memory.ExtendedWords
import           Foreign.Storable
import           Foreign.Ptr

import           Prelude hiding (length, take, drop, span, concat, replicate, splitAt, null, pred)

-- | Transform a bytearray at a specific offset into
-- a Word64 tagged as BE (Big Endian)
--
-- no bounds checking. unsafe
toW64BE :: ByteArrayAccess bs => bs -> Int -> BE Word64
toW64BE bs ofs = unsafeDoIO $ withByteArray bs $ \p -> peek (p `plusPtr` ofs)

-- | Transform a bytearray at a specific offset into
-- a Word64 tagged as LE (Little Endian)
--
-- no bounds checking. unsafe
toW64LE :: ByteArrayAccess bs => bs -> Int -> LE Word64
toW64LE bs ofs = unsafeDoIO $ withByteArray bs $ \p -> peek (p `plusPtr` ofs)

-- | Serialize a @Word64@ to a @ByteArray@ in big endian format
fromW64BE :: (ByteArray ba) => Word64 -> ba
fromW64BE n = allocAndFreeze 8 $ \p -> poke p (toBE n)

-- | map blocks of 128 bits of a bytearray, creating a new bytestring
-- of equivalent size where each blocks has been mapped through @f@
--
-- no length checking is done. unsafe
mapAsWord128 :: ByteArray bs => (Word128 -> Word128) -> bs -> bs
mapAsWord128 f bs =
    unsafeCreate len $ \dst ->
    withByteArray bs $ \src ->
        loop (len `div` 16) dst src
  where
        len        = length bs
        loop :: Int -> Ptr (BE Word64) -> Ptr (BE Word64) -> IO ()
        loop 0 _ _ = return ()
        loop i d s = do
            w1 <- peek s
            w2 <- peek (s `plusPtr` 8)
            let (Word128 r1 r2) = f (Word128 (fromBE w1) (fromBE w2))
            poke d               (toBE r1)
            poke (d `plusPtr` 8) (toBE r2)
            loop (i-1) (d `plusPtr` 16) (s `plusPtr` 16)

-- | map blocks of 64 bits of a bytearray, creating a new bytestring
-- of equivalent size where each blocks has been mapped through @f@
--
-- no length checking is done. unsafe
mapAsWord64 :: ByteArray bs => (Word64 -> Word64) -> bs -> bs
mapAsWord64 f bs =
    unsafeCreate len $ \dst ->
    withByteArray bs $ \src ->
        loop (len `div` 8) dst src
  where
        len        = length bs

        loop :: Int -> Ptr (BE Word64) -> Ptr (BE Word64) -> IO ()
        loop 0 _ _ = return ()
        loop i d s = do
            w <- peek s
            let r = f (fromBE w)
            poke d (toBE r)
            loop (i-1) (d `plusPtr` 8) (s `plusPtr` 8)
