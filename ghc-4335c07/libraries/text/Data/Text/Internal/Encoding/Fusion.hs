{-# LANGUAGE BangPatterns, CPP, Rank2Types #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Fusion
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fusible 'Stream'-oriented functions for converting between 'Text'
-- and several common encodings.

module Data.Text.Internal.Encoding.Fusion
    (
    -- * Streaming
      streamASCII
    , streamUtf8
    , streamUtf16LE
    , streamUtf16BE
    , streamUtf32LE
    , streamUtf32BE

    -- * Unstreaming
    , unstream

    , module Data.Text.Internal.Encoding.Fusion.Common
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.ByteString.Internal (ByteString(..), mallocByteString, memcpy)
import Data.Text.Internal.Fusion (Step(..), Stream(..))
import Data.Text.Internal.Fusion.Size
import Data.Text.Encoding.Error
import Data.Text.Internal.Encoding.Fusion.Common
import Data.Text.Internal.Unsafe.Char (unsafeChr, unsafeChr8, unsafeChr32)
import Data.Text.Internal.Unsafe.Shift (shiftL, shiftR)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Storable (pokeByteOff)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Encoding.Utf32 as U32
import Data.Text.Unsafe (unsafeDupablePerformIO)

streamASCII :: ByteString -> Stream Char
streamASCII bs = Stream next 0 (maxSize l)
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l    = Done
          | otherwise = Yield (unsafeChr8 x1) (i+1)
          where
            x1 = B.unsafeIndex bs i
{-# DEPRECATED streamASCII "Do not use this function" #-}
{-# INLINE [0] streamASCII #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using UTF-8
-- encoding.
streamUtf8 :: OnDecodeError -> ByteString -> Stream Char
streamUtf8 onErr bs = Stream next 0 (maxSize l)
    where
      l = B.length bs
      next i
          | i >= l = Done
          | U8.validate1 x1 = Yield (unsafeChr8 x1) (i+1)
          | i+1 < l && U8.validate2 x1 x2 = Yield (U8.chr2 x1 x2) (i+2)
          | i+2 < l && U8.validate3 x1 x2 x3 = Yield (U8.chr3 x1 x2 x3) (i+3)
          | i+3 < l && U8.validate4 x1 x2 x3 x4 = Yield (U8.chr4 x1 x2 x3 x4) (i+4)
          | otherwise = decodeError "streamUtf8" "UTF-8" onErr (Just x1) (i+1)
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = B.unsafeIndex bs
{-# INLINE [0] streamUtf8 #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
streamUtf16LE :: OnDecodeError -> ByteString -> Stream Char
streamUtf16LE onErr bs = Stream next 0 (maxSize (l `shiftR` 1))
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = decodeError "streamUtf16LE" "UTF-16LE" onErr Nothing (i+1)
          where
            x1    = idx i       + (idx (i + 1) `shiftL` 8)
            x2    = idx (i + 2) + (idx (i + 3) `shiftL` 8)
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] streamUtf16LE #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-16 encoding.
streamUtf16BE :: OnDecodeError -> ByteString -> Stream Char
streamUtf16BE onErr bs = Stream next 0 (maxSize (l `shiftR` 1))
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = decodeError "streamUtf16BE" "UTF-16BE" onErr Nothing (i+1)
          where
            x1    = (idx i `shiftL` 8)       + idx (i + 1)
            x2    = (idx (i + 2) `shiftL` 8) + idx (i + 3)
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] streamUtf16BE #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-32 encoding.
streamUtf32BE :: OnDecodeError -> ByteString -> Stream Char
streamUtf32BE onErr bs = Stream next 0 (maxSize (l `shiftR` 2))
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                    = Done
          | i+3 < l && U32.validate x = Yield (unsafeChr32 x) (i+4)
          | otherwise = decodeError "streamUtf32BE" "UTF-32BE" onErr Nothing (i+1)
          where
            x     = shiftL x1 24 + shiftL x2 16 + shiftL x3 8 + x4
            x1    = idx i
            x2    = idx (i+1)
            x3    = idx (i+2)
            x4    = idx (i+3)
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE [0] streamUtf32BE #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-32 encoding.
streamUtf32LE :: OnDecodeError -> ByteString -> Stream Char
streamUtf32LE onErr bs = Stream next 0 (maxSize (l `shiftR` 2))
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                    = Done
          | i+3 < l && U32.validate x = Yield (unsafeChr32 x) (i+4)
          | otherwise = decodeError "streamUtf32LE" "UTF-32LE" onErr Nothing (i+1)
          where
            x     = shiftL x4 24 + shiftL x3 16 + shiftL x2 8 + x1
            x1    = idx i
            x2    = idx $ i+1
            x3    = idx $ i+2
            x4    = idx $ i+3
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE [0] streamUtf32LE #-}

-- | /O(n)/ Convert a 'Stream' 'Word8' to a 'ByteString'.
unstream :: Stream Word8 -> ByteString
unstream (Stream next s0 len) = unsafeDupablePerformIO $ do
    let mlen = upperBound 4 len
    mallocByteString mlen >>= loop mlen 0 s0
    where
      loop !n !off !s fp = case next s of
          Done -> trimUp fp n off
          Skip s' -> loop n off s' fp
          Yield x s'
              | off == n -> realloc fp n off s' x
              | otherwise -> do
            withForeignPtr fp $ \p -> pokeByteOff p off x
            loop n (off+1) s' fp
      {-# NOINLINE realloc #-}
      realloc fp n off s x = do
        let n' = n+n
        fp' <- copy0 fp n n'
        withForeignPtr fp' $ \p -> pokeByteOff p off x
        loop n' (off+1) s fp'
      {-# NOINLINE trimUp #-}
      trimUp fp _ off = return $! PS fp 0 off
      copy0 :: ForeignPtr Word8 -> Int -> Int -> IO (ForeignPtr Word8)
      copy0 !src !srcLen !destLen =
#if defined(ASSERTS)
        assert (srcLen <= destLen) $
#endif
        do
          dest <- mallocByteString destLen
          withForeignPtr src  $ \src'  ->
              withForeignPtr dest $ \dest' ->
                  memcpy dest' src' (fromIntegral srcLen)
          return dest

decodeError :: forall s. String -> String -> OnDecodeError -> Maybe Word8
            -> s -> Step s Char
decodeError func kind onErr mb i =
    case onErr desc mb of
      Nothing -> Skip i
      Just c  -> Yield c i
    where desc = "Data.Text.Internal.Encoding.Fusion." ++ func ++ ": Invalid " ++
                 kind ++ " stream"
