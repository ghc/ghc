{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Base64.URL.Lazy
-- Copyright   : (c) 2012 Ian Lynagh
--
-- License     : BSD-style
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>,
--               Herbert Valerio Riedel <hvr@gnu.org>,
--               Mikhail Glushenkov <mikhail.glushenkov@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded
-- lazy bytestrings.
--
-- @since 1.0.0.0
module Data.ByteString.Base64.URL.Lazy
    (
      encode
    , encodeUnpadded
    , decode
    , decodeUnpadded
    , decodePadded
    , decodeLenient
    ) where

import Data.ByteString.Base64.Internal
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char

-- | Encode a string into base64 form.  The result will always be a
-- multiple of 4 bytes in length.
encode :: L.ByteString -> L.ByteString
encode = L.fromChunks . map B64.encode . reChunkIn 3 . L.toChunks

-- | Encode a string into unpadded base64url form.
--
-- @since 1.1.0.0
encodeUnpadded :: L.ByteString -> L.ByteString
encodeUnpadded = L.fromChunks
    . map B64.encodeUnpadded
    . reChunkIn 3
    . L.toChunks

-- | Decode a base64-encoded string.  This function strictly follows
-- the specification in
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>.
decode :: L.ByteString -> Either String L.ByteString
decode b = -- Returning an Either type means that the entire result will
           -- need to be in memory at once anyway, so we may as well
           -- keep it simple and just convert to and from a strict byte
           -- string
           -- TODO: Use L.{fromStrict,toStrict} once we can rely on
           -- a new enough bytestring
           case B64.decode $ S.concat $ L.toChunks b of
           Left err -> Left err
           Right b' -> Right $ L.fromChunks [b']

-- | Decode a unpadded base64url-encoded string, failing if input is padded.
-- This function follows the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>
-- and in <https://tools.ietf.org/html/rfc7049#section-2.4.4.2 RFC 7049 2.4>
--
-- @since 1.1.0.0
decodeUnpadded :: L.ByteString -> Either String L.ByteString
decodeUnpadded bs = case B64.decodeUnpadded $ S.concat $ L.toChunks bs of
  Right b -> Right $ L.fromChunks [b]
  Left e -> Left e

-- | Decode a padded base64url-encoded string, failing if input is improperly padded.
-- This function follows the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>
-- and in <https://tools.ietf.org/html/rfc7049#section-2.4.4.2 RFC 7049 2.4>
--
-- @since 1.1.0.0
decodePadded :: L.ByteString -> Either String L.ByteString
decodePadded bs = case B64.decodePadded $ S.concat $ L.toChunks bs of
  Right b -> Right $ L.fromChunks [b]
  Left e -> Left e

-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not generate
-- parse errors no matter how poor its input.
decodeLenient :: L.ByteString -> L.ByteString
decodeLenient = L.fromChunks . map B64.decodeLenient . reChunkIn 4 . L.toChunks
              . LC.filter goodChar
    where -- We filter out and '=' padding here, but B64.decodeLenient
          -- handles that
          goodChar c = isAlphaNum c || c == '-' || c == '_'
