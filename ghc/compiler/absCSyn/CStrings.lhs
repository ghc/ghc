This module deals with printing C string literals 

\begin{code}
module CStrings(
	CLabelString, isCLabelString, pprCLabelString,

	pp_cSEP,

	pprFSInCStyle, pprStringInCStyle
  ) where

#include "HsVersions.h"

import Char	( ord, chr, isAlphaNum )
import FastString
import Outputable
\end{code}


\begin{code}
type CLabelString = FastString		-- A C label, completely unencoded

pprCLabelString :: CLabelString -> SDoc
pprCLabelString lbl = ftext lbl

isCLabelString :: CLabelString -> Bool	-- Checks to see if this is a valid C label
isCLabelString lbl 
  = all ok (unpackFS lbl)
  where
    ok c = isAlphaNum c || c == '_' || c == '.'
	-- The '.' appears in e.g. "foo.so" in the 
	-- module part of a ExtName.  Maybe it should be separate

pp_cSEP = char '_'
\end{code}

\begin{code}
pprFSInCStyle :: FastString -> SDoc
-- Assumes it contains only characters '\0'..'\xFF'!
pprFSInCStyle fs = pprStringInCStyle (unpackFS fs)

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
