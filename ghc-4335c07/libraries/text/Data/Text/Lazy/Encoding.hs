{-# LANGUAGE BangPatterns,CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      : Data.Text.Lazy.Encoding
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for converting lazy 'Text' values to and from lazy
-- 'ByteString', using several standard encodings.
--
-- To gain access to a much larger variety of encodings, use the
-- @text-icu@ package: <http://hackage.haskell.org/package/text-icu>

module Data.Text.Lazy.Encoding
    (
    -- * Decoding ByteStrings to Text
    -- $strict
      decodeASCII
    , decodeLatin1
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- ** Catchable failure
    , decodeUtf8'

    -- ** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    -- * Encoding Text using ByteString Builders
    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped
    ) where

import Control.Exception (evaluate, try)
import Data.Monoid (Monoid(..))
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)
import Data.Text.Internal.Lazy (Text(..), chunk, empty, foldrChunks)
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B (safeStrategy, toLazyByteStringWith)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal.Lazy.Encoding.Fusion as E
import qualified Data.Text.Internal.Lazy.Fusion as F
import Data.Text.Unsafe (unsafeDupablePerformIO)

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- | /Deprecated/.  Decode a 'ByteString' containing 7-bit ASCII
-- encoded text.
decodeASCII :: B.ByteString -> Text
decodeASCII = decodeUtf8
{-# DEPRECATED decodeASCII "Use decodeUtf8 instead" #-}

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
decodeLatin1 :: B.ByteString -> Text
decodeLatin1 = foldr (chunk . TE.decodeLatin1) empty . B.toChunks

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> B.ByteString -> Text
decodeUtf8With onErr (B.Chunk b0 bs0) =
    case TE.streamDecodeUtf8With onErr b0 of
      TE.Some t l f -> chunk t (go f l bs0)
  where
    go f0 _ (B.Chunk b bs) =
      case f0 b of
        TE.Some t l f -> chunk t (go f l bs)
    go _ l _
      | S.null l  = empty
      | otherwise = case onErr desc (Just (B.unsafeHead l)) of
                      Nothing -> empty
                      Just c  -> Chunk (T.singleton c) Empty
    desc = "Data.Text.Lazy.Encoding.decodeUtf8With: Invalid UTF-8 stream"
decodeUtf8With _ _ = empty

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
decodeUtf8 :: B.ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE[0] decodeUtf8 #-}

-- This rule seems to cause performance loss.
{- RULES "LAZY STREAM stream/decodeUtf8' fusion" [1]
   forall bs. F.stream (decodeUtf8' bs) = E.streamUtf8 strictDecode bs #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text..
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
--
-- /Note/: this function is /not/ lazy, as it must decode its entire
-- input before it can return a result.  If you need lazy (streaming)
-- decoding, use 'decodeUtf8With' in lenient mode.
decodeUtf8' :: B.ByteString -> Either UnicodeException Text
decodeUtf8' bs = unsafeDupablePerformIO $ do
                   let t = decodeUtf8 bs
                   try (evaluate (rnf t `seq` t))
  where
    rnf Empty        = ()
    rnf (Chunk _ ts) = rnf ts
{-# INLINE decodeUtf8' #-}

encodeUtf8 :: Text -> B.ByteString
encodeUtf8    Empty       = B.empty
encodeUtf8 lt@(Chunk t _) =
    B.toLazyByteStringWith strategy B.empty $ encodeUtf8Builder lt
  where
    -- To improve our small string performance, we use a strategy that
    -- allocates a buffer that is guaranteed to be large enough for the
    -- encoding of the first chunk, but not larger than the default
    -- B.smallChunkSize. We clamp the firstChunkSize to ensure that we don't
    -- generate too large buffers which hamper streaming.
    firstChunkSize  = min B.smallChunkSize (4 * (T.length t + 1))
    strategy        = B.safeStrategy firstChunkSize B.defaultChunkSize

encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder =
    foldrChunks (\c b -> TE.encodeUtf8Builder c `mappend` b) Data.Monoid.mempty

{-# INLINE encodeUtf8BuilderEscaped #-}
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped prim =
    foldrChunks (\c b -> TE.encodeUtf8BuilderEscaped prim c `mappend` b) mempty

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: B.ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: B.ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> B.ByteString
encodeUtf16LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16LE) [] txt)
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> B.ByteString
encodeUtf16BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16BE) [] txt)
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: B.ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: B.ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> B.ByteString
encodeUtf32LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32LE) [] txt)
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> B.ByteString
encodeUtf32BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32BE) [] txt)
{-# INLINE encodeUtf32BE #-}
