{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Char
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Char type and associated operations.
--
-----------------------------------------------------------------------------

module Data.Char 
    (
      Char

    , isAscii, isLatin1, isControl
    , isPrint, isSpace,  isUpper
    , isLower, isAlpha,  isDigit
    , isOctDigit, isHexDigit, isAlphaNum  -- :: Char -> Bool

    , toUpper, toLower  -- :: Char -> Char

    , digitToInt        -- :: Char -> Int
    , intToDigit        -- :: Int  -> Char

    , ord               -- :: Char -> Int
    , chr               -- :: Int  -> Char
    , readLitChar       -- :: ReadS Char 
    , showLitChar       -- :: Char -> ShowS
    , lexLitChar	-- :: ReadS String

    , String

     -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Show
import GHC.Read (readLitChar, lexLitChar)
import GHC.Unicode
import GHC.Num
#endif

#ifdef __HUGS__
import Hugs.Char
#endif

#ifdef __NHC__
import Prelude
import Prelude(Char,String)
import Char
#endif


digitToInt :: Char -> Int
digitToInt c
 | isDigit c		=  ord c - ord '0'
 | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
 | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
 | otherwise	        =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh
