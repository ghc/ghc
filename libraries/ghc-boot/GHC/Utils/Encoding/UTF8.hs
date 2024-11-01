{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, MultiWayIf #-}
{-# OPTIONS_GHC -O2 -fno-warn-name-shadowing #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected. This module used to live in the `ghc`
-- package but has been moved to `ghc-boot` because the definition
-- of the package database (needed in both ghc and in ghc-pkg) lives in
-- `ghc-boot` and uses ShortText, which in turn depends on this module.

-- | Simple, non-streaming Modified UTF-8 codecs.
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
module GHC.Utils.Encoding.UTF8
    ( -- * Decoding single characters
      utf8DecodeCharAddr#
    , utf8DecodeCharPtr
    , utf8DecodeCharByteArray#
    , utf8PrevChar
    , utf8CharStart
    , utf8UnconsByteString
      -- * Decoding strings
    , utf8DecodeByteString
    , utf8DecodeShortByteString
    , utf8DecodeForeignPtr
    , utf8DecodeByteArray#
      -- * Counting characters
    , utf8CountCharsShortByteString
    , utf8CountCharsByteArray#
      -- * Comparison
    , utf8CompareByteArray#
    , utf8CompareShortByteString
      -- * Encoding strings
    , utf8EncodeByteArray#
    , utf8EncodePtr
    , utf8EncodeByteString
    , utf8EncodeShortByteString
    , utf8EncodedLength
    ) where


import Prelude

import Foreign
import GHC.IO
import GHC.Encoding.UTF8

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Short.Internal (ShortByteString(..))

-- | Find the start of the codepoint preceding the codepoint at the given
-- 'Ptr'. This is undefined if there is no previous valid codepoint.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

-- | Find the start of the codepoint at the given 'Ptr'. This is undefined if
-- there is no previous valid codepoint.
utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
                 if w >= 0x80 && w < 0xC0
                        then go (p `plusPtr` (-1))
                        else return p

utf8CountCharsShortByteString :: ShortByteString -> Int
utf8CountCharsShortByteString (SBS ba) = utf8CountCharsByteArray# ba

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#) = utf8DecodeByteArray# ba#

-- | Decode a 'ByteString' containing a UTF-8 string.
utf8DecodeByteString :: ByteString -> [Char]
utf8DecodeByteString (BS.PS fptr offset len)
  = utf8DecodeForeignPtr fptr offset len

utf8EncodeShortByteString :: String -> ShortByteString
utf8EncodeShortByteString str = SBS (utf8EncodeByteArray# str)

-- | Encode a 'String' into a 'ByteString'.
utf8EncodeByteString :: String -> ByteString
utf8EncodeByteString s =
  unsafePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodePtr ptr s
      pure (BS.fromForeignPtr buf 0 len)

utf8UnconsByteString :: ByteString -> Maybe (Char, ByteString)
utf8UnconsByteString (BS.PS _ _ 0) = Nothing
utf8UnconsByteString (BS.PS fptr offset len)
  = unsafeDupablePerformIO $
      withForeignPtr fptr $ \ptr -> do
        let (c,n) = utf8DecodeCharPtr (ptr `plusPtr` offset)
        return $ Just (c, BS.PS fptr (offset + n) (len - n))

utf8CompareShortByteString :: ShortByteString -> ShortByteString -> Ordering
utf8CompareShortByteString (SBS a1) (SBS a2) = utf8CompareByteArray# a1 a2
