{-# LANGUAGE NoImplicitPrelude, PatternGuards #-}
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
    isSurrogate, recoverDecode, recoverEncode
  ) where

import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Exception

import GHC.Base
import GHC.Word
import GHC.Show
import GHC.Num
import GHC.Real ( fromIntegral )

--import System.Posix.Internals

import Data.Maybe

-- | The 'CodingFailureMode' is used to construct 'TextEncoding's, and specifies
-- how they handle illegal sequences.
data CodingFailureMode = ErrorOnCodingFailure         -- ^ Throw an error when an illegal sequence is encountered
                       | IgnoreCodingFailure          -- ^ Attempt to ignore and recover if an illegal sequence is encountered
                       | TransliterateCodingFailure   -- ^ Replace with the closest visual match upon an illegal sequence
                       | SurrogateEscapeFailure       -- ^ Use the surrogate escape mechanism to attempt to allow illegal sequences to be roundtripped.
                       deriving (Show)                -- This will only work properly for those encodings which are strict supersets of ASCII in the sense
                                                      -- that valid ASCII data is also valid in that encoding. This is not true for e.g. UTF-16, because
                                                      -- ASCII characters must be padded to two bytes to retain their meaning.

codingFailureModeSuffix :: CodingFailureMode -> String
codingFailureModeSuffix ErrorOnCodingFailure       = ""
codingFailureModeSuffix IgnoreCodingFailure        = "//IGNORE"
codingFailureModeSuffix TransliterateCodingFailure = "//TRANSLIT"
codingFailureModeSuffix SurrogateEscapeFailure     = "//SURROGATE"

-- | In transliterate mode, we use this character when decoding unknown bytes.
--
-- This is the defined Unicode replacement character: <http://www.fileformat.info/info/unicode/char/0fffd/index.htm>
unrepresentableChar :: Char
unrepresentableChar = '\xFFFD'

-- | Some characters are actually "surrogate" codepoints defined for use in UTF-16. We need to signal an
-- invalid character if we detect them when encoding a sequence of 'Char's into 'Word8's because the
-- 'SurrogateEscapeFailure' mode creates unpaired surrogates to round-trip bytes through our internal
-- UTF-16 encoding.
isSurrogate :: Char -> Bool
isSurrogate c = (0xD800 <= x && x <= 0xDBFF) || (0xDC00 <= x && x <= 0xDFFF)
  where x = ord c

escapeToSurrogateCharacter :: Word8 -> Char
escapeToSurrogateCharacter b
  | b < 128   = chr (fromIntegral b) -- Disallow 'smuggling' of ASCII bytes. For roundtripping to work, this assumes encoding is ASCII-superset.
  | otherwise = chr (0xDC00 + fromIntegral b)

unescapeSurrogateCharacter :: Char -> Maybe Word8
unescapeSurrogateCharacter c
    | 0xDC80 <= x && x < 0xDD00 = Just (fromIntegral x) -- Discard high byte
    | otherwise                 = Nothing
  where x = ord c

recoverDecode :: CodingFailureMode -> Buffer Word8 -> Buffer Char -> IO (Buffer Word8, Buffer Char)
recoverDecode cfm input@Buffer{  bufRaw=iraw, bufL=ir, bufR=_  }
                  output@Buffer{ bufRaw=oraw, bufL=_,  bufR=ow } = do
 --puts $ "recoverDecode " ++ show ir
 case cfm of
  ErrorOnCodingFailure       -> ioe_decodingError
  IgnoreCodingFailure        -> return (input { bufL=ir+1 }, output)
  TransliterateCodingFailure -> do
      ow' <- writeCharBuf oraw ow unrepresentableChar
      return (input { bufL=ir+1 }, output { bufR=ow' })
  SurrogateEscapeFailure     -> do
      b <- readWord8Buf iraw ir
      ow' <- writeCharBuf oraw ow (escapeToSurrogateCharacter b)
      return (input { bufL=ir+1 }, output { bufR=ow' })

recoverEncode :: CodingFailureMode -> Buffer Char -> Buffer Word8 -> IO (Buffer Char, Buffer Word8)
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
          -- XXX: evil hack! To implement transliteration, we just poke an
          -- ASCII ? into the input buffer and tell the caller to try and decode
          -- again. This is *probably* safe given current uses of TextEncoding.
          --
          -- The "if" test above ensures we skip if the encoding fails to deal with
          -- the ?, though this should never happen in practice as all encodings are
          -- in fact capable of reperesenting all ASCII characters.
          _ir' <- writeCharBuf iraw ir '?'
          return (input, output)
        
        -- This implementation does not work because e.g. UTF-16 requires 2 bytes to
        -- encode a simple ASCII value
        --writeWord8Buf oraw ow unrepresentableByte
        --return (input { bufL=ir' }, output { bufR=ow+1 })
    SurrogateEscapeFailure | Just x <- unescapeSurrogateCharacter c -> do
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
