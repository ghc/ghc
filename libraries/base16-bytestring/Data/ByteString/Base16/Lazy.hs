{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      : Data.ByteString.Base16.Lazy
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : Herbert Valerio Riedel <hvr@gnu.org>,
--               Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,
--               Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : stable
-- Portability : non-portable
--
-- RFC 4648-compliant Base16 (Hexadecimal) encoding for lazy 'ByteString' values.
-- For a complete Base16 encoding specification, please see <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>.
--
module Data.ByteString.Base16.Lazy
( encode
, decode
, decodeLenient
) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Base16.Internal
import Data.ByteString.Lazy.Internal (ByteString(..))

-- | Encode a 'ByteString' value in base16 (i.e. hexadecimal).
-- Encoded values will always have a length that is a multiple of 2.
--
--
-- === __Examples__:
--
-- > encode "foo"  == "666f6f"
--
encode :: ByteString -> ByteString
encode Empty = Empty
encode (Chunk c cs) = Chunk (B16.encode c) (encode cs)

-- | Decode a base16-encoded 'ByteString' value.
-- If errors are encountered during the decoding process,
-- then an error message and character offset will be returned in
-- the @Left@ clause of the coproduct.
--
-- === __Examples__:
--
-- > decode "666f6f" == Right "foo"
-- > decode "66quux" == Left "invalid character at offset: 2"
-- > decode "666quu" == Left "invalid character at offset: 3"
--
-- @since 1.0.0.0
--
decode :: ByteString -> Either String ByteString
decode = f . B16.decode . BS.concat . LBS.toChunks
  where
    f (Left t) = Left t
    f (Right bs') = Right (LBS.fromChunks [bs'])

-- | Decode a Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails.
--
-- /N.B./: this is not RFC 4648-compliant
--
-- === __Examples__:
--
-- > decodeLenient "666f6f" == "foo"
-- > decodeLenient "66quux" == "f"
-- > decodeLenient "666quu" == "f"
-- > decodeLenient "666fqu" == "fo"
--
-- @since 1.0.0.0
--
decodeLenient :: ByteString -> ByteString
decodeLenient = LBS.fromChunks
    . fmap B16.decodeLenient
    . reChunk
    . fmap (BS.filter (flip BS.elem extendedHex))
    . LBS.toChunks
  where
    extendedHex = BS.pack (fmap c2w "0123456789abcdefABCDEF")
