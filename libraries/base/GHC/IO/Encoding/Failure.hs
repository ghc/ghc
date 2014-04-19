{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Failure
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Types for specifying how text encoding/decoding fails
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.Failure (
    CodingFailureMode(..), codingFailureModeSuffix,
    isSurrogate,
    recoverDecode, recoverEncode
  ) where

import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Exception

import GHC.Base
import GHC.Char
import GHC.Word
import GHC.Show
import GHC.Num
import GHC.Real ( fromIntegral )

--import System.Posix.Internals

import Data.Maybe


-- | The 'CodingFailureMode' is used to construct 'TextEncoding's, and
-- specifies how they handle illegal sequences.
data CodingFailureMode
  = ErrorOnCodingFailure
       -- ^ Throw an error when an illegal sequence is encountered
  | IgnoreCodingFailure
       -- ^ Attempt to ignore and recover if an illegal sequence is
       -- encountered
  | TransliterateCodingFailure
       -- ^ Replace with the closest visual match upon an illegal
       -- sequence
  | RoundtripFailure
       -- ^ Use the private-use escape mechanism to attempt to allow
       -- illegal sequences to be roundtripped.
  deriving (Show)
       -- This will only work properly for those encodings which are
       -- strict supersets of ASCII in the sense that valid ASCII data
       -- is also valid in that encoding. This is not true for
       -- e.g. UTF-16, because ASCII characters must be padded to two
       -- bytes to retain their meaning.

-- Note [Roundtripping]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Roundtripping is based on the ideas of PEP383.
--
-- We used to use the range of private-use characters from 0xEF80 to
-- 0xEFFF designated for "encoding hacks" by the ConScript Unicode Registery
-- to encode these characters.
--
-- However, people didn't like this because it means we don't get
-- guaranteed roundtripping for byte sequences that look like a UTF-8
-- encoded codepoint 0xEFxx.
--
-- So now like PEP383 we use lone surrogate codepoints 0xDCxx to escape
-- undecodable bytes, even though that may confuse Unicode processing
-- software written in Haskell. This guarantees roundtripping because
-- unicode input that includes lone surrogate codepoints is invalid by
-- definition.
--
-- When we used private-use characters there was a technical problem when it
-- came to encoding back to bytes using iconv. The iconv code will not fail when
-- it tries to encode a private-use character (as it would if trying to encode
-- a surrogate), which means that we won't get a chance to replace it
-- with the byte we originally escaped.
--
-- To work around this, when filling the buffer to be encoded (in
-- writeBlocks/withEncodedCString/newEncodedCString), we replaced the
-- private-use characters with lone surrogates again! Likewise, when
-- reading from a buffer (unpack/unpack_nl/peekEncodedCString) we have
-- to do the inverse process.
--
-- The user of String would never see these lone surrogates, but it
-- ensures that iconv will throw an error when encountering them.  We
-- use lone surrogates in the range 0xDC00 to 0xDCFF for this purpose.

codingFailureModeSuffix :: CodingFailureMode -> String
codingFailureModeSuffix ErrorOnCodingFailure       = ""
codingFailureModeSuffix IgnoreCodingFailure        = "//IGNORE"
codingFailureModeSuffix TransliterateCodingFailure = "//TRANSLIT"
codingFailureModeSuffix RoundtripFailure           = "//ROUNDTRIP"

-- | In transliterate mode, we use this character when decoding
-- unknown bytes.
--
-- This is the defined Unicode replacement character:
-- <http://www.fileformat.info/info/unicode/char/0fffd/index.htm>
unrepresentableChar :: Char
unrepresentableChar = '\xFFFD'

-- It is extraordinarily important that this series of
-- predicates/transformers gets inlined, because they tend to be used
-- in inner loops related to text encoding. In particular,
-- surrogatifyRoundtripCharacter must be inlined (see #5536)

-- | Some characters are actually "surrogate" codepoints defined for
-- use in UTF-16. We need to signal an invalid character if we detect
-- them when encoding a sequence of 'Char's into 'Word8's because they
-- won't give valid Unicode.
--
-- We may also need to signal an invalid character if we detect them
-- when encoding a sequence of 'Char's into 'Word8's because the
-- 'RoundtripFailure' mode creates these to round-trip bytes through
-- our internal UTF-16 encoding.
{-# INLINE isSurrogate #-}
isSurrogate :: Char -> Bool
isSurrogate c = (0xD800 <= x && x <= 0xDBFF)
             || (0xDC00 <= x && x <= 0xDFFF)
  where x = ord c

-- Bytes (in Buffer Word8) --> lone surrogates (in Buffer CharBufElem)
{-# INLINE escapeToRoundtripCharacterSurrogate #-}
escapeToRoundtripCharacterSurrogate :: Word8 -> Char
escapeToRoundtripCharacterSurrogate b
  | b < 128   = chr (fromIntegral b)
      -- Disallow 'smuggling' of ASCII bytes. For roundtripping to
      -- work, this assumes encoding is ASCII-superset.
  | otherwise = chr (0xDC00 + fromIntegral b)

-- Lone surrogates (in Buffer CharBufElem) --> bytes (in Buffer Word8)
{-# INLINE unescapeRoundtripCharacterSurrogate #-}
unescapeRoundtripCharacterSurrogate :: Char -> Maybe Word8
unescapeRoundtripCharacterSurrogate c
    | 0xDC80 <= x && x < 0xDD00 = Just (fromIntegral x) -- Discard high byte
    | otherwise                 = Nothing
  where x = ord c

recoverDecode :: CodingFailureMode -> Buffer Word8 -> Buffer Char
              -> IO (Buffer Word8, Buffer Char)
recoverDecode cfm input@Buffer{  bufRaw=iraw, bufL=ir, bufR=_  }
                  output@Buffer{ bufRaw=oraw, bufL=_,  bufR=ow } = do
 --puts $ "recoverDecode " ++ show ir
 case cfm of
  ErrorOnCodingFailure       -> ioe_decodingError
  IgnoreCodingFailure        -> return (input { bufL=ir+1 }, output)
  TransliterateCodingFailure -> do
      ow' <- writeCharBuf oraw ow unrepresentableChar
      return (input { bufL=ir+1 }, output { bufR=ow' })
  RoundtripFailure           -> do
      b <- readWord8Buf iraw ir
      ow' <- writeCharBuf oraw ow (escapeToRoundtripCharacterSurrogate b)
      return (input { bufL=ir+1 }, output { bufR=ow' })

recoverEncode :: CodingFailureMode -> Buffer Char -> Buffer Word8
              -> IO (Buffer Char, Buffer Word8)
recoverEncode cfm input@Buffer{  bufRaw=iraw, bufL=ir, bufR=_  }
                  output@Buffer{ bufRaw=oraw, bufL=_,  bufR=ow } = do
  (c,ir') <- readCharBuf iraw ir
  --puts $ "recoverEncode " ++ show ir ++ " " ++ show ir'
  case cfm of
    IgnoreCodingFailure        -> return (input { bufL=ir' }, output)
    TransliterateCodingFailure -> do
        if c == '?'
         then return (input { bufL=ir' }, output)
         else do
          -- XXX: evil hack! To implement transliteration, we just
          -- poke an ASCII ? into the input buffer and tell the caller
          -- to try and decode again. This is *probably* safe given
          -- current uses of TextEncoding.
          --
          -- The "if" test above ensures we skip if the encoding fails
          -- to deal with the ?, though this should never happen in
          -- practice as all encodings are in fact capable of
          -- reperesenting all ASCII characters.
          _ir' <- writeCharBuf iraw ir '?'
          return (input, output)
        
        -- This implementation does not work because e.g. UTF-16
        -- requires 2 bytes to encode a simple ASCII value
        --writeWord8Buf oraw ow unrepresentableByte
        --return (input { bufL=ir' }, output { bufR=ow+1 })
    RoundtripFailure | Just x <- unescapeRoundtripCharacterSurrogate c -> do
        writeWord8Buf oraw ow x
        return (input { bufL=ir' }, output { bufR=ow+1 })
    _                          -> ioe_encodingError

ioe_decodingError :: IO a
ioe_decodingError = ioException
    (IOError Nothing InvalidArgument "recoverDecode"
        "invalid byte sequence" Nothing Nothing)

ioe_encodingError :: IO a
ioe_encodingError = ioException
    (IOError Nothing InvalidArgument "recoverEncode"
        "invalid character" Nothing Nothing)

