%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Char]{Module @Char@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Char 
    ( 
     isAscii, isLatin1, isControl, 
     isPrint, isSpace,  isUpper, 
     isLower, isAlpha,  isDigit,  
     isOctDigit, isHexDigit, isAlphanum,  -- :: Char -> Bool

     toUpper, toLower,  -- :: Char -> Char

     digitToInt,        -- :: Char -> Int
     intToDigit,        -- :: Int  -> Char

     ord,               -- :: Char -> Int
     chr,               -- :: Int  -> Char
     readLitChar,       -- :: ReadS Char 
     showLitChar        -- :: Char -> ShowS
    ) where

import PrelBase
import PrelRead (readLitChar)
import GHCerr   ( error )

\end{code}

\begin{code}
-- Digit conversion operations

digitToInt :: Char -> Int
digitToInt c
 | isDigit c		=  fromEnum c - fromEnum '0'
 | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
 | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
 | otherwise	        =  error "Char.digitToInt: not a digit" -- sigh


\end{code}
