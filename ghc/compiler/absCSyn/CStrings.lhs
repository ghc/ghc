This module deals with printing C string literals 

\begin{code}
module CStrings(
	CLabelString, isCLabelString, pprCLabelString,

	cSEP, pp_cSEP,

	pprFSInCStyle, pprStringInCStyle
  ) where

#include "HsVersions.h"

import Char	( ord, chr, isAlphaNum )
import Outputable
\end{code}


\begin{code}
type CLabelString = FAST_STRING		-- A C label, completely unencoded

pprCLabelString lbl = ptext lbl

isCLabelString :: CLabelString -> Bool	-- Checks to see if this is a valid C label
isCLabelString lbl 
  = all ok (_UNPK_ lbl)
  where
    ok c = isAlphaNum c || c == '_' || c == '.'
	-- The '.' appears in e.g. "foo.so" in the 
	-- module part of a ExtName.  Maybe it should be separate

cSEP    = SLIT("_")	-- official C separator
pp_cSEP = char '_'
\end{code}

\begin{code}
pprFSInCStyle :: FAST_STRING -> SDoc
-- Assumes it contains only characters '\0'..'\xFF'!
pprFSInCStyle fs = pprStringInCStyle (_UNPK_ fs)

pprStringInCStyle :: String -> SDoc
pprStringInCStyle s = doubleQuotes (text (concatMap charToC s))

charToC :: Char -> String
charToC '\"' = "\\\""
charToC '\'' = "\\\'"
charToC '\\' = "\\\\"
charToC c | c >= ' ' && c <= '~' = [c]
          | c > '\xFF' = panic ("charToC "++show c)
          | otherwise = ['\\',
                         chr (ord '0' + ord c `div` 64),
                         chr (ord '0' + ord c `div` 8 `mod` 8),
                         chr (ord '0' + ord c         `mod` 8)]
\end{code}
