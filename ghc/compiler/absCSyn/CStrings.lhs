This module deals with printing (a) C string literals and (b) C labels.

\begin{code}
#include "HsVersions.h"

module CStrings(

	cSEP,
	pp_cSEP,

	identToC, modnameToC,
	stringToC, charToC,
	charToEasyHaskell

  ) where

CHK_Ubiq() -- debugging consistency check

import Pretty
import Unpretty( uppChar )

IMPORT_1_3(Char (isAlphanum))
#ifdef REALLY_HASKELL_1_3
ord = fromEnum :: Char -> Int
chr = toEnum   :: Int -> Char
#endif
\end{code}


\begin{verbatim}
_ is the main separator

orig		becomes
****		*******
_		Zu
'		Zq (etc for ops ??)
<funny char>	Z[hex-digit][hex-digit]
Prelude<x>	ZP<x>
<std class>	ZC<?>
<std tycon>	ZT<?>
\end{verbatim}

\begin{code}
cSEP    = SLIT("_")	-- official C separator
pp_cSEP = uppChar '_'

identToC    :: FAST_STRING -> Pretty
modnameToC  :: FAST_STRING -> FAST_STRING
stringToC   :: String -> String
charToC, charToEasyHaskell :: Char -> String

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
	  _    -> '\\' : 'o' : (octify (ord c))

octify :: Int -> String
octify n
  = if n < 8 then
	[chr (n + ord '0')]
    else
	octify (n `quot` 8) ++ [chr (n `rem` 8 + ord '0')]

identToC ps
  = let
	str = _UNPK_ ps
    in
    ppBeside
	(case str of
	   's':'t':'d':_ -> -- avoid "stdin", "stdout", and "stderr"...
			    ppChar 'Z'
	   _	         -> ppNil)

	(if (all isAlphanum str) -- we gamble that this test will succeed...
	 then ppPStr ps
	 else ppIntersperse ppNil (map char_to_c str))
  where
    char_to_c 'Z'  = ppPStr SLIT("ZZ")
    char_to_c '&'  = ppPStr SLIT("Za")
    char_to_c '|'  = ppPStr SLIT("Zb")
    char_to_c ':'  = ppPStr SLIT("Zc")
    char_to_c '/'  = ppPStr SLIT("Zd")
    char_to_c '='  = ppPStr SLIT("Ze")
    char_to_c '>'  = ppPStr SLIT("Zg")
    char_to_c '#'  = ppPStr SLIT("Zh")
    char_to_c '<'  = ppPStr SLIT("Zl")
    char_to_c '-'  = ppPStr SLIT("Zm")
    char_to_c '!'  = ppPStr SLIT("Zn")
    char_to_c '.'  = ppPStr SLIT("Zo")
    char_to_c '+'  = ppPStr SLIT("Zp")
    char_to_c '\'' = ppPStr SLIT("Zq")
    char_to_c '*'  = ppPStr SLIT("Zt")
    char_to_c '_'  = ppPStr SLIT("Zu")

    char_to_c c    = if isAlphanum c
		     then ppChar c
		     else ppBeside (ppChar 'Z') (ppInt (ord c))
\end{code}

For \tr{modnameToC}, we really only have to worry about \tr{'}s (quote
chars) in the name.  Rare.
\begin{code}
modnameToC ps
  = let
	str = _UNPK_ ps
    in
    if not (any quote_here str) then
	ps
    else
	_PK_ (concat (map char_to_c str))
  where
    quote_here '\'' = True
    quote_here _    = False

    char_to_c c
      = if isAlphanum c then [c] else 'Z' : (show (ord c))
\end{code}


