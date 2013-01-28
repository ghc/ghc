{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf
-- Copyright   :  (c) Lennart Augustsson, 2004-2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  lennart@augustsson.net
-- Stability   :  provisional
-- Portability :  portable
--
-- A C printf like formatter.
--
-----------------------------------------------------------------------------

{-# Language CPP #-}

module Text.Printf(
   printf, hPrintf,
   PrintfType, HPrintfType, PrintfArg, IsChar
) where

import Prelude
import Data.Char
import Data.Int
import Data.Word
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
-- >    +      always use a sign (+ or -) for signed conversions
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
-- >    c      character               Char, Int, Integer, ...
-- >    d      decimal                 Char, Int, Integer, ...
-- >    o      octal                   Char, Int, Integer, ...
-- >    x      hexadecimal             Char, Int, Integer, ...
-- >    X      hexadecimal             Char, Int, Integer, ...
-- >    u      unsigned decimal        Char, Int, Integer, ...
-- >    f      floating point          Float, Double
-- >    g      general format float    Float, Double
-- >    G      general format float    Float, Double
-- >    e      exponent format float   Float, Double
-- >    E      exponent format float   Float, Double
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
printf fmts = spr fmts []

-- | Similar to 'printf', except that output is via the specified
-- 'Handle'.  The return type is restricted to @('IO' a)@.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmts = hspr hdl fmts []

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

{- not allowed in Haskell 2010
instance PrintfType String where
    spr fmt args = uprintf fmt (reverse args)
-}
instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

instance PrintfType (IO a) where
    spr fmts args = do
	putStr (uprintf fmts (reverse args))
	return (error "PrintfType (IO a): result should not be used.")

instance HPrintfType (IO a) where
    hspr hdl fmts args = do
	hPutStr hdl (uprintf fmts (reverse args))
	return (error "HPrintfType (IO a): result should not be used.")

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts (toUPrintf a : args)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmts args = \ a -> hspr hdl fmts (toUPrintf a : args)

class PrintfArg a where
    toUPrintf :: a -> UPrintf

instance PrintfArg Char where
    toUPrintf c = UChar c

{- not allowed in Haskell 2010
instance PrintfArg String where
    toUPrintf s = UString s
-}
instance (IsChar c) => PrintfArg [c] where
    toUPrintf = UString . map toChar

instance PrintfArg Int where
    toUPrintf = uInteger

instance PrintfArg Int8 where
    toUPrintf = uInteger

instance PrintfArg Int16 where
    toUPrintf = uInteger

instance PrintfArg Int32 where
    toUPrintf = uInteger

instance PrintfArg Int64 where
    toUPrintf = uInteger

#ifndef __NHC__
instance PrintfArg Word where
    toUPrintf = uInteger
#endif

instance PrintfArg Word8 where
    toUPrintf = uInteger

instance PrintfArg Word16 where
    toUPrintf = uInteger

instance PrintfArg Word32 where
    toUPrintf = uInteger

instance PrintfArg Word64 where
    toUPrintf = uInteger

instance PrintfArg Integer where
    toUPrintf = UInteger 0

instance PrintfArg Float where
    toUPrintf = UFloat

instance PrintfArg Double where
    toUPrintf = UDouble

uInteger :: (Integral a, Bounded a) => a -> UPrintf
uInteger x = UInteger (toInteger $ minBound `asTypeOf` x) (toInteger x)

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c

-------------------

data UPrintf = UChar Char | UString String | UInteger Integer Integer | UFloat Float | UDouble Double

uprintf :: String -> [UPrintf] -> String
uprintf ""       []       = ""
uprintf ""       (_:_)    = fmterr
uprintf ('%':'%':cs) us   = '%':uprintf cs us
uprintf ('%':_)  []       = argerr
uprintf ('%':cs) us@(_:_) = fmt cs us
uprintf (c:cs)   us       = c:uprintf cs us

fmt :: String -> [UPrintf] -> String
fmt cs us =
	let (width, prec, ladj, zero, plus, cs', us') = getSpecs False False False cs us
	    adjust (pre, str) = 
		let lstr = length str
		    lpre = length pre
		    fill = if lstr+lpre < width then take (width-(lstr+lpre)) (repeat (if zero then '0' else ' ')) else ""
		in  if ladj then pre ++ str ++ fill else if zero then pre ++ fill ++ str else fill ++ pre ++ str
            adjust' ("", str) | plus = adjust ("+", str)
            adjust' ps = adjust ps
        in
	case cs' of
	[]     -> fmterr
	c:cs'' ->
	    case us' of
	    []     -> argerr
	    u:us'' ->
		(case c of
		'c' -> adjust  ("", [toEnum (toint u)])
		'd' -> adjust' (fmti prec u)
		'i' -> adjust' (fmti prec u)
		'x' -> adjust  ("", fmtu 16 prec u)
		'X' -> adjust  ("", map toUpper $ fmtu 16 prec u)
		'o' -> adjust  ("", fmtu 8  prec u)
		'u' -> adjust  ("", fmtu 10 prec u)
		'e' -> adjust' (dfmt' c prec u)
		'E' -> adjust' (dfmt' c prec u)
		'f' -> adjust' (dfmt' c prec u)
		'g' -> adjust' (dfmt' c prec u)
		'G' -> adjust' (dfmt' c prec u)
		's' -> adjust  ("", tostr prec u)
		_   -> perror ("bad formatting char " ++ [c])
		 ) ++ uprintf cs'' us''

fmti :: Int -> UPrintf -> (String, String)
fmti prec (UInteger _ i) = if i < 0 then ("-", integral_prec prec (show (-i))) else ("", integral_prec prec (show i))
fmti _ (UChar c)         = fmti 0 (uInteger (fromEnum c))
fmti _ _                 = baderr

fmtu :: Integer -> Int -> UPrintf -> String
fmtu b prec (UInteger l i) = integral_prec prec (itosb b (if i < 0 then -2*l + i else i))
fmtu b _    (UChar c)      = itosb b (toInteger (fromEnum c))
fmtu _ _ _                 = baderr

integral_prec :: Int -> String -> String
integral_prec prec integral = (replicate (prec - (length integral)) '0') ++ integral

toint :: UPrintf -> Int
toint (UInteger _ i) = fromInteger i
toint (UChar c)      = fromEnum c
toint _		     = baderr

tostr :: Int -> UPrintf -> String
tostr n (UString s) = if n >= 0 then take n s else s
tostr _ _		  = baderr

itosb :: Integer -> Integer -> String
itosb b n = 
	if n < b then 
	    [intToDigit $ fromInteger n]
	else
	    let (q, r) = quotRem n b in
	    itosb b q ++ [intToDigit $ fromInteger r]

stoi :: Int -> String -> (Int, String)
stoi a (c:cs) | isDigit c = stoi (a*10 + digitToInt c) cs
stoi a cs                 = (a, cs)

getSpecs :: Bool -> Bool -> Bool -> String -> [UPrintf] -> (Int, Int, Bool, Bool, Bool, String, [UPrintf])
getSpecs _ z s ('-':cs) us = getSpecs True z s cs us
getSpecs l z _ ('+':cs) us = getSpecs l z True cs us
getSpecs l _ s ('0':cs) us = getSpecs l True s cs us
getSpecs l z s ('*':cs) us =
	let (us', n) = getStar us
	    ((p, cs''), us'') =
		    case cs of
                    '.':'*':r -> let (us''', p') = getStar us'
		    	      	 in  ((p', r), us''')
		    '.':r     -> (stoi 0 r, us')
		    _         -> ((-1, cs), us')
	in  (abs n, p, if n < 0 then not l else l, z, s, cs'', us'')
getSpecs l z s ('.':cs) us =
	let ((p, cs'), us') = 
	        case cs of
		'*':cs'' -> let (us'', p') = getStar us in ((p', cs''), us'')
                _ ->        (stoi 0 cs, us)
	in  (0, p, l, z, s, cs', us')
getSpecs l z s cs@(c:_) us | isDigit c =
	let (n, cs') = stoi 0 cs
	    ((p, cs''), us') = case cs' of
	    	 	       '.':'*':r -> let (us'', p') = getStar us in ((p', r), us'')
		               '.':r -> (stoi 0 r, us)
			       _     -> ((-1, cs'), us)
	in  (n, p, l, z, s, cs'', us')
getSpecs l z s cs       us = (0, -1, l, z, s, cs, us)

getStar :: [UPrintf] -> ([UPrintf], Int)
getStar us =
    case us of
    [] -> argerr
    nu : us' -> (us', toint nu)


dfmt' :: Char -> Int -> UPrintf -> (String, String)
dfmt' c p (UDouble d) = dfmt c p d
dfmt' c p (UFloat f)  = dfmt c p f
dfmt' _ _ _           = baderr

dfmt :: (RealFloat a) => Char -> Int -> a -> (String, String)
dfmt c p d =
	case (if isUpper c then map toUpper else id) $
             (case toLower c of
                  'e' -> showEFloat
                  'f' -> showFFloat
                  'g' -> showGFloat
                  _   -> error "Printf.dfmt: impossible"
             )
               (if p < 0 then Nothing else Just p) d "" of
	'-':cs -> ("-", cs)
	cs     -> ("" , cs)

perror :: String -> a
perror s = error ("Printf.printf: "++s)
fmterr, argerr, baderr :: a
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"

