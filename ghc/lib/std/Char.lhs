% -----------------------------------------------------------------------------
% $Id: Char.lhs,v 1.7 2000/06/30 13:39:35 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
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

#ifndef __HUGS__
import PrelBase
import PrelShow
import PrelEnum
import PrelNum
import PrelRead (readLitChar, lexLitChar, digitToInt)
import PrelErr  ( error )
#else
isLatin1 c = True
#endif
\end{code}
