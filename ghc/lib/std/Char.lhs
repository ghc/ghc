%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Char]{Module @Char@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Char 
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

import PrelBase
import PrelRead (readLitChar, lexLitChar)
import {-# SOURCE #-} PrelErr   ( error )

\end{code}

\begin{code}
-- Digit conversion operations

digitToInt :: Char -> Int
digitToInt c
 | isDigit c		=  fromEnum c - fromEnum '0'
 | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
 | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
 | otherwise	        =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh


\end{code}
