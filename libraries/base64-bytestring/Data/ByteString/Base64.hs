{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Base64
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>,
--               Herbert Valerio Riedel <hvr@gnu.org>,
--               Mikhail Glushenkov <mikhail.glushenkov@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded strings.
--
-- @since 0.1.0.0
module Data.ByteString.Base64
  ( encode
  , decode
  , decodeLenient
  ) where

import Data.ByteString.Base64.Internal
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

-- | Encode a string into base64 form.  The result will always be a
-- multiple of 4 bytes in length.
encode :: ByteString -> ByteString
encode s = encodeWith Padded (mkEncodeTable alphabet) s

-- | Decode a base64-encoded string. This function strictly follows
-- the specification in
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>.
--
-- (Note: this means that even @"\\n"@ and @"\\r\\n"@ as line breaks are rejected
-- rather than ignored.  If you are using this in the context of a
-- standard that overrules RFC 4648 such as HTTP multipart mime bodies,
-- consider using 'decodeLenient'.)
decode :: ByteString -> Either String ByteString
decode s = decodeWithTable Padded decodeFP s

-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not
-- generate parse errors no matter how poor its input.
decodeLenient :: ByteString -> ByteString
decodeLenient s = decodeLenientWithTable decodeFP s

alphabet :: ByteString
alphabet = B.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]
{-# NOINLINE alphabet #-}

decodeFP :: ForeignPtr Word8
#if MIN_VERSION_bytestring(0,11,0)
BS decodeFP _ =
#else
PS decodeFP _ _ =
#endif
  B.pack $ replicate 43 x
    ++ [62,x,x,x,63]
    ++ [52..61]
    ++ [x,x,x,done,x,x,x]
    ++ [0..25]
    ++ [x,x,x,x,x,x]
    ++ [26..51]
    ++ replicate 133 x
{-# NOINLINE decodeFP #-}

x :: Integral a => a
x = 255
{-# INLINE x #-}
