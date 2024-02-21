{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Unicode
-- Copyright   :  (c) The University of Glasgow, 2003
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Implementations for the character predicates (isLower, isUpper, etc.)
-- and the conversions (toUpper, toLower).  The implementation uses
-- libunicode on Unix systems if that is available.
--

module GHC.Unicode
    (unicodeVersion,
     GeneralCategory(..),
     generalCategory,
     isAscii,
     isLatin1,
     isControl,
     isAsciiUpper,
     isAsciiLower,
     isPrint,
     isSpace,
     isUpper,
     isUpperCase,
     isLower,
     isLowerCase,
     isAlpha,
     isDigit,
     isOctDigit,
     isHexDigit,
     isAlphaNum,
     isPunctuation,
     isSymbol,
     toUpper,
     toLower,
     toTitle
     ) where

import GHC.Internal.Unicode
