{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Base64.URL
-- Copyright   : (c) 2012 Deian Stefan
--
-- License     : BSD-style
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>,
--               Herbert Valerio Riedel <hvr@gnu.org>,
--               Mikhail Glushenkov <mikhail.glushenkov@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64url-encoded strings.
--
-- @since 0.1.1.0
module Data.ByteString.Base64.URL
  ( encode
  , encodeUnpadded
  , decode
  , decodePadded
  , decodeUnpadded
  , decodeLenient
  ) where

import Data.ByteString.Base64.Internal
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

-- | Encode a string into base64url form.  The result will always be a
-- multiple of 4 bytes in length.
encode :: ByteString -> ByteString
encode = encodeWith Padded (mkEncodeTable alphabet)

-- | Encode a string into unpadded base64url form.
--
-- @since 1.1.0.0
encodeUnpadded :: ByteString -> ByteString
encodeUnpadded = encodeWith Unpadded (mkEncodeTable alphabet)

-- | Decode a base64url-encoded string applying padding if necessary.
-- This function follows the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>
-- and in <https://tools.ietf.org/html/rfc7049#section-2.4.4.2 RFC 7049 2.4>
decode :: ByteString -> Either String ByteString
decode = decodeWithTable Don'tCare decodeFP

-- | Decode a padded base64url-encoded string, failing if input is improperly padded.
-- This function follows the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>
-- and in <https://tools.ietf.org/html/rfc7049#section-2.4.4.2 RFC 7049 2.4>
--
-- @since 1.1.0.0
decodePadded :: ByteString -> Either String ByteString
decodePadded = decodeWithTable Padded decodeFP

-- | Decode a unpadded base64url-encoded string, failing if input is padded.
-- This function follows the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>
-- and in <https://tools.ietf.org/html/rfc7049#section-2.4.4.2 RFC 7049 2.4>
--
-- @since 1.1.0.0
decodeUnpadded :: ByteString -> Either String ByteString
decodeUnpadded = decodeWithTable Unpadded decodeFP

-- | Decode a base64url-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not
-- generate parse errors no matter how poor its input.
decodeLenient :: ByteString -> ByteString
decodeLenient = decodeLenientWithTable decodeFP


alphabet :: ByteString
alphabet = B.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [45,95]
{-# NOINLINE alphabet #-}

decodeFP :: ForeignPtr Word8
#if MIN_VERSION_bytestring(0,11,0)
BS decodeFP _ =
#else
PS decodeFP _ _ =
#endif
  B.pack $ replicate 45 x
    ++ [62,x,x]
    ++ [52..61]
    ++ [x,x,x,done,x,x,x]
    ++ [0..25]
    ++ [x,x,x,x,63,x]
    ++ [26..51]
    ++ replicate 133 x

{-# NOINLINE decodeFP #-}

x :: Integral a => a
x = 255
{-# INLINE x #-}
