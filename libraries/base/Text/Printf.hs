-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf
-- Copyright   :  (c) Lennart Augustsson, 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  lennart@augustsson.net
-- Stability   :  provisional
-- Portability :  portable
--
-- A C printf like formatter.
--
-----------------------------------------------------------------------------

module Text.Printf(
   printf, hPrintf,
   PrintfType, HPrintfType, PrintfArg, IsChar
) where

import Prelude
import Data.Array
import Data.Char
import Numeric(showEFloat, showFFloat, showGFloat)
import System.IO

-------------------

-- | Format a variable number of arguments with the C-style formatting string.
-- The return value is either 'String' or @('IO' a)@.
--
-- The format string consists of ordinary characters and /conversion
-- specifications/, which specify how to format one of the arguments
-- to printf in the output string.  A conversion specification begins with the
-- character @%@, followed by one or more of the following flags:
--
-- >    -      left adjust (default is right adjust)
-- >    0      pad with zeroes rather than spaces
--
-- followed optionally by a field width:
-- 
-- >    num    field width
-- >    *      as num, but taken from argument list
--
-- followed optionally by a precision:
--
-- >    .num   precision (number of decimal places)
--
-- and finally, a format character:
--
-- >    c      character               Char, Int, Integer
-- >    d      decimal                 Char, Int, Integer
-- >    o      octal                   Char, Int, Integer
-- >    x      hexadecimal             Char, Int, Integer
-- >    u      unsigned decimal        Char, Int, Integer
-- >    f      floating point          Float, Double
-- >    g      general format float    Float, Double
-- >    e      exponent format float   Float, Double
-- >    s      string                  String
--
-- Mismatch between the argument types and the format string will cause
-- an exception to be thrown at runtime.
--
-- Examples:
--
-- >   > printf "%d\n" (23::Int)
-- >   23
-- >   > printf "%s %s\n" "Hello" "World"
-- >   Hello World
-- >   > printf "%.2f\n" pi
-- >   3.14
--
printf :: (PrintfType r) => String -> r
printf fmt = spr fmt []

-- | Similar to 'printf', except that output is via the specified
-- 'Handle'.  The return type is restricted to @('IO' a)@.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmt = hspr hdl fmt []

-- |The 'PrintfType' class provides the variable argument magic for
-- 'printf'.  Its implementation is intentionally not visible from
-- this module. If you attempt to pass an argument of a type which
-- is not an instance of this class to 'printf' or 'hPrintf', then
-- the compiler will report it as a missing instance of 'PrintfArg'.
class PrintfType t where
    spr :: String -> [UPrintf] -> t

-- | The 'HPrintfType' class provides the variable argument magic for
-- 'hPrintf'.  Its implementation is intentionally not visible from
-- this module.
class HPrintfType t where
    hspr :: Handle -> String -> [UPrintf] -> t

{- not allowed in Haskell 98
instance PrintfType String where
    spr fmt args = uprintf fmt (reverse args)
-}
instance (IsChar c) => PrintfType [c] where
    spr fmt args = map fromChar (uprintf fmt (reverse args))

instance PrintfType (IO a) where
    spr fmt args = do
	putStr (uprintf fmt (reverse args))
	return undefined

instance HPrintfType (IO a) where
    hspr hdl fmt args = do
	hPutStr hdl (uprintf fmt (reverse args))
	return undefined

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmt args = \ a -> spr fmt (toUPrintf a : args)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmt args = \ a -> hspr hdl fmt (toUPrintf a : args)

class PrintfArg a where
    toUPrintf :: a -> UPrintf

instance PrintfArg Char where
    toUPrintf c = UChar c

{- not allowed in Haskell 98
instance PrintfArg String where
    toUPrintf s = UString s
-}
instance (IsChar c) => PrintfArg [c] where
    toUPrintf s = UString (map toChar s)

instance PrintfArg Int where
    toUPrintf i = UInt i

instance PrintfArg Integer where
    toUPrintf i = UInteger i

instance PrintfArg Float where
    toUPrintf f = UFloat f

instance PrintfArg Double where
    toUPrintf d = UDouble d

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c

-------------------

data UPrintf = UChar Char | UString String | UInt Int | UInteger Integer | UFloat Float | UDouble Double

uprintf :: String -> [UPrintf] -> String
uprintf ""       []       = ""
uprintf ""       (_:_)    = fmterr
uprintf ('%':'%':cs) us   = '%':uprintf cs us
uprintf ('%':_)  []       = argerr
uprintf ('%':cs) us@(_:_) = fmt cs us
uprintf (c:cs)   us       = c:uprintf cs us

fmt :: String -> [UPrintf] -> String
fmt cs us =
	let (width, prec, ladj, zero, cs', us') = getSpecs False False cs us
	    adjust (pre, str) = 
		let lstr = length str
		    lpre = length pre
		    fill = if lstr+lpre < width then take (width-(lstr+lpre)) (repeat (if zero then '0' else ' ')) else ""
		in  if ladj then pre ++ str ++ fill else if zero then pre ++ fill ++ str else fill ++ pre ++ str
        in
	case cs' of
	[]     -> fmterr
	c:cs'' ->
	    case us' of
	    []     -> argerr
	    u:us'' ->
		(case c of
		'c' -> adjust ("", [toEnum (toint u)])
		'd' -> adjust (fmti u)
		'x' -> adjust ("", fmtu 16 u)
		'o' -> adjust ("", fmtu 8  u)
		'u' -> adjust ("", fmtu 10 u)
		'e' -> adjust (dfmt' c prec u)
		'f' -> adjust (dfmt' c prec u)
		'g' -> adjust (dfmt' c prec u)
		's' -> adjust ("", tostr u)
		c   -> perror ("bad formatting char " ++ [c])
		 ) ++ uprintf cs'' us''

fmti (UInt i)     = if i < 0 then
			if i == -i then fmti (UInteger (toInteger i)) else ("-", itos (-i))
		    else
			("", itos i)
fmti (UInteger i) = if i < 0 then ("-", itos (-i)) else ("", itos i)
fmti (UChar c)    = fmti (UInt (fromEnum c))
fmti u		  = baderr

fmtu b (UInt i)     = if i < 0 then
			  if i == -i then itosb b (maxi - toInteger (i+1) - 1) else itosb b (maxi - toInteger (-i))
		      else
			  itosb b (toInteger i)
fmtu b (UInteger i) = itosb b i
fmtu b (UChar c)    = itosb b (toInteger (fromEnum c))
fmtu b u            = baderr

maxi :: Integer
maxi = (toInteger (maxBound::Int) + 1) * 2

toint (UInt i)     = i
toint (UInteger i) = toInt i
toint (UChar c)    = fromEnum c
toint u		   = baderr

tostr (UString s) = s
tostr u		  = baderr

itos n = 
	if n < 10 then 
	    [toEnum (fromEnum '0' + toInt n)]
	else
	    let (q, r) = quotRem n 10 in
	    itos q ++ [toEnum (fromEnum '0' + toInt r)]

chars = array (0,15) (zipWith (,) [0..] "0123456789abcdef")
itosb :: Integer -> Integer -> String
itosb b n = 
	if n < b then 
	    [chars!n]
	else
	    let (q, r) = quotRem n b in
	    itosb b q ++ [chars!r]

stoi :: Int -> String -> (Int, String)
stoi a (c:cs) | isDigit c = stoi (a*10 + fromEnum c - fromEnum '0') cs
stoi a cs                 = (a, cs)

getSpecs :: Bool -> Bool -> String -> [UPrintf] -> (Int, Int, Bool, Bool, String, [UPrintf])
getSpecs l z ('-':cs) us = getSpecs True z cs us
getSpecs l z ('0':cs) us = getSpecs l True cs us
getSpecs l z ('*':cs) us = 
        case us of
        [] -> argerr
        nu : us' ->
	    let n = toint nu
		(p, cs'', us'') =
		    case cs of
                    '.':'*':r -> case us' of { [] -> argerr; pu:us'' -> (toint pu, r, us'') }
		    '.':r     -> let (n, cs') = stoi 0 r in (n, cs', us')
		    _         -> (-1, cs, us')
	    in  (n, p, l, z, cs'', us'')
getSpecs l z ('.':cs) us =
	let (p, cs') = stoi 0 cs
	in  (0, p, l, z, cs', us)
getSpecs l z cs@(c:_) us | isDigit c =
	let (n, cs') = stoi 0 cs
	    (p, cs'') = case cs' of
			'.':r -> stoi 0 r
			_     -> (-1, cs')
	in  (n, p, l, z, cs'', us)
getSpecs l z cs       us = (0, -1, l, z, cs, us)

dfmt' c p (UDouble d) = dfmt c p d
dfmt' c p (UFloat f)  = dfmt c p f
dfmt' c p u           = baderr

dfmt c p d = 
	case (case c of 'e' -> showEFloat; 'f' -> showFFloat; 'g' -> showGFloat) 
               (if p < 0 then Nothing else Just p) d "" of
	'-':cs -> ("-", cs)
	cs     -> ("" , cs)

perror s = error ("Printf.printf: "++s)
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"

toInt :: (Integral a) => a -> Int
toInt x = fromInteger (toInteger x)
