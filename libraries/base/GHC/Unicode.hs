{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Unicode
-- Copyright   :  (c) The University of Glasgow, 2003
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Implementations for the character predicates (isLower, isUpper, etc.)
-- and the conversions (toUpper, toLower).  The implementation uses
-- libunicode on Unix systems if that is available.
--
-----------------------------------------------------------------------------

module GHC.Unicode (
        isAscii, isLatin1, isControl,
        isAsciiUpper, isAsciiLower,
        isPrint, isSpace,  isUpper,
        isLower, isAlpha,  isDigit,
        isOctDigit, isHexDigit, isAlphaNum,
        toUpper, toLower, toTitle,
        wgencat
    ) where

import GHC.Base
import GHC.Char
import GHC.Real        (fromIntegral)
import Foreign.C.Types (CInt(..))

#include "HsBaseConfig.h"

-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
isAscii                 :: Char -> Bool
isAscii c               =  c <  '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1                :: Char -> Bool
isLatin1 c              =  c <= '\xff'

-- | Selects ASCII lower-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower :: Char -> Bool
isAsciiLower c          =  c >= 'a' && c <= 'z'

-- | Selects ASCII upper-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper :: Char -> Bool
isAsciiUpper c          =  c >= 'A' && c <= 'Z'

-- | Selects control characters, which are the non-printing characters of
-- the Latin-1 subset of Unicode.
isControl               :: Char -> Bool

-- | Selects printable Unicode characters
-- (letters, numbers, marks, punctuation, symbols and spaces).
isPrint                 :: Char -> Bool

-- | Returns 'True' for any Unicode space character, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with GHC.List elem
isSpace c               =  c == ' '     ||
                           c == '\t'    ||
                           c == '\n'    ||
                           c == '\r'    ||
                           c == '\f'    ||
                           c == '\v'    ||
                           c == '\xa0'  ||
                           iswspace (fromIntegral (ord c)) /= 0

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the
-- single-character form of /Lj/.
isUpper                 :: Char -> Bool

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower                 :: Char -> Bool

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and modifiers letters).
-- This function is equivalent to 'Data.Char.isLetter'.
isAlpha                 :: Char -> Bool

-- | Selects alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are selected by this
-- function but not by 'isDigit'.  Such digits may be part of identifiers
-- but are not used by the printer and reader to represent numbers.
isAlphaNum              :: Char -> Bool

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit                 :: Char -> Bool
isDigit c               =  c >= '0' && c <= '9'

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit              :: Char -> Bool
isOctDigit c            =  c >= '0' && c <= '7'

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit              :: Char -> Bool
isHexDigit c            =  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'

-- | Convert a letter to the corresponding upper-case letter, if any.
-- Any other character is returned unchanged.
toUpper                 :: Char -> Char

-- | Convert a letter to the corresponding lower-case letter, if any.
-- Any other character is returned unchanged.
toLower                 :: Char -> Char

-- | Convert a letter to the corresponding title-case or upper-case
-- letter, if any.  (Title case differs from upper case only for a small
-- number of ligature letters.)
-- Any other character is returned unchanged.
toTitle                 :: Char -> Char

-- -----------------------------------------------------------------------------
-- Implementation with the supplied auto-generated Unicode character properties
-- table

-- Regardless of the O/S and Library, use the functions contained in WCsubst.c

isAlpha    c = iswalpha (fromIntegral (ord c)) /= 0
isAlphaNum c = iswalnum (fromIntegral (ord c)) /= 0
--isSpace    c = iswspace (fromIntegral (ord c)) /= 0
isControl  c = iswcntrl (fromIntegral (ord c)) /= 0
isPrint    c = iswprint (fromIntegral (ord c)) /= 0
isUpper    c = iswupper (fromIntegral (ord c)) /= 0
isLower    c = iswlower (fromIntegral (ord c)) /= 0

toLower c = chr (fromIntegral (towlower (fromIntegral (ord c))))
toUpper c = chr (fromIntegral (towupper (fromIntegral (ord c))))
toTitle c = chr (fromIntegral (towtitle (fromIntegral (ord c))))

foreign import ccall unsafe "u_iswalpha"
  iswalpha :: CInt -> CInt

foreign import ccall unsafe "u_iswalnum"
  iswalnum :: CInt -> CInt

foreign import ccall unsafe "u_iswcntrl"
  iswcntrl :: CInt -> CInt

foreign import ccall unsafe "u_iswspace"
  iswspace :: CInt -> CInt

foreign import ccall unsafe "u_iswprint"
  iswprint :: CInt -> CInt

foreign import ccall unsafe "u_iswlower"
  iswlower :: CInt -> CInt

foreign import ccall unsafe "u_iswupper"
  iswupper :: CInt -> CInt

foreign import ccall unsafe "u_towlower"
  towlower :: CInt -> CInt

foreign import ccall unsafe "u_towupper"
  towupper :: CInt -> CInt

foreign import ccall unsafe "u_towtitle"
  towtitle :: CInt -> CInt

foreign import ccall unsafe "u_gencat"
  wgencat :: CInt -> CInt

