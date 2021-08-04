{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Base64.Lazy
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
module Data.ByteString.Base64.Lazy
    (
      encode
    , decode
    , decodeLenient
    ) where

import Data.ByteString.Base64.Internal
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char

-- | Encode a string into base64 form.  The result will always be a
-- multiple of 4 bytes in length.
encode :: L.ByteString -> L.ByteString
encode = L.fromChunks . map B64.encode . reChunkIn 3 . L.toChunks

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

-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not generate
-- parse errors no matter how poor its input.
decodeLenient :: L.ByteString -> L.ByteString
decodeLenient = L.fromChunks . map B64.decodeLenient . reChunkIn 4 . L.toChunks
              . LC.filter goodChar
    where -- We filter out and '=' padding here, but B64.decodeLenient
          -- handles that
          goodChar c = isDigit c || isAsciiUpper c || isAsciiLower c
                                 || c == '+' || c == '/'
