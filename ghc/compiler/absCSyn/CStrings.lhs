This module deals with printing C string literals 

\begin{code}
module CStrings(
	cSEP, pp_cSEP,

	stringToC, charToC, pprFSInCStyle,
	charToEasyHaskell
  ) where

#include "HsVersions.h"

import Char	( ord, chr )
import Outputable
\end{code}


\begin{code}
cSEP    = SLIT("_")	-- official C separator
pp_cSEP = char '_'

stringToC   :: String -> String
charToC, charToEasyHaskell :: Char -> String

pprFSInCStyle :: FAST_STRING -> SDoc
pprFSInCStyle fs = doubleQuotes (text (stringToC (_UNPK_ fs)))

-- stringToC: the hassle is what to do w/ strings like "ESC 0"...

stringToC ""  = ""
stringToC [c] = charToC c
stringToC (c:cs)
    -- if we have something "octifiable" in "c", we'd better "octify"
    -- the rest of the string, too.
  = if (c < ' ' || c > '~')
    then (charToC c) ++ (concat (map char_to_C cs))
    else (charToC c) ++ (stringToC cs)
  where
    char_to_C c | c == '\n' = "\\n"	-- use C escapes when we can
		| c == '\a' = "\\a"
		| c == '\b' = "\\b"	-- ToDo: chk some of these...
		| c == '\r' = "\\r"
		| c == '\t' = "\\t"
		| c == '\f' = "\\f"
		| c == '\v' = "\\v"
		| otherwise = '\\' : (octify (ord c))

charToC c = if (c >= ' ' && c <= '~')	-- non-portable...
	    then case c of
		  '\'' -> "\\'"
		  '\\' -> "\\\\"
		  '"'  -> "\\\""
		  '\n' -> "\\n"
		  '\a' -> "\\a"
		  '\b' -> "\\b"
		  '\r' -> "\\r"
		  '\t' -> "\\t"
		  '\f' -> "\\f"
		  '\v' -> "\\v"
		  _    -> [c]
	    else '\\' : (octify (ord c))

-- really: charToSimpleHaskell

charToEasyHaskell c
  = if (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    then [c]
    else case c of
	  _    -> '\\' : show (ord c)

octify :: Int -> String
octify n
  = if n < 8 then
	[chr (n + ord '0')]
    else
	octify (n `quot` 8) ++ [chr (n `rem` 8 + ord '0')]
\end{code}

