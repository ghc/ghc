--
-- A C printf like formatter.
-- Conversion specs:
--	-	left adjust
--	num	field width
--      *       as num, but taken from argument list
--	.	separates width from precision
-- Formatting characters:
-- 	c	Char, Int, Integer
--	d	Char, Int, Integer
--	o	Char, Int, Integer
--	x	Char, Int, Integer
--	u	Char, Int, Integer
--	f	Float, Double
--	g	Float, Double
--	e	Float, Double
--	s	String
--
module Printf(UPrintf(..), printf) where

#if defined(__HBC__)
import LMLfmtf
#endif

#if defined(__YALE_HASKELL__)
import PrintfPrims
#endif

#if defined(__GLASGOW_HASKELL__)
import PreludeGlaST
import TyArray		( _ByteArray(..) )
#endif

data UPrintf = UChar Char | UString String | UInt Int | UInteger Integer | UFloat Float | UDouble Double

printf :: String -> [UPrintf] -> String
printf ""       []       = ""
printf ""       (_:_)    = fmterr
printf ('%':'%':cs) us   = '%':printf cs us
printf ('%':_)  []       = argerr
printf ('%':cs) us@(_:_) = fmt cs us
printf (c:cs)   us       = c:printf cs us

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
		'c' -> adjust ("", [chr (toint u)])
		'd' -> adjust (fmti u)
		'x' -> adjust ("", fmtu 16 u)
		'o' -> adjust ("", fmtu 8  u)
		'u' -> adjust ("", fmtu 10 u)
#if defined __YALE_HASKELL__
		'e' -> adjust (fmte prec (todbl u))
		'f' -> adjust (fmtf prec (todbl u))
		'g' -> adjust (fmtg prec (todbl u))
#else
		'e' -> adjust (dfmt c prec (todbl u))
		'f' -> adjust (dfmt c prec (todbl u))
		'g' -> adjust (dfmt c prec (todbl u))
#endif
		's' -> adjust ("", tostr u)
		c   -> perror ("bad formatting char " ++ [c])
		) ++ printf cs'' us''

fmti (UInt i)     = if i < 0 then
			if i == -i then fmti (UInteger (toInteger i)) else ("-", itos (-i))
		    else
			("", itos i)
fmti (UInteger i) = if i < 0 then ("-", itos (-i)) else ("", itos i)
fmti (UChar c)    = fmti (UInt (ord c))
fmti u		  = baderr

fmtu b (UInt i)     = if i < 0 then
			  if i == -i then itosb b (maxi - toInteger (i+1) - 1) else itosb b (maxi - toInteger (-i))
		      else
			  itosb b (toInteger i)
fmtu b (UInteger i) = itosb b i
fmtu b (UChar c)    = itosb b (toInteger (ord c))
fmtu b u            = baderr

maxi :: Integer
maxi = (toInteger maxInt + 1) * 2

toint (UInt i)     = i
toint (UInteger i) = toInt i
toint (UChar c)    = ord c
toint u		   = baderr

tostr (UString s) = s
tostr u		  = baderr

todbl (UDouble d)     = d
#if defined(__GLASGOW_HASKELL__)
todbl (UFloat (F# f)) = D# (float2Double# f) -- What a great system(TM) !
#else
todbl (UFloat f)      = fromRational (toRational f)
#endif
todbl u		      = baderr

itos n = 
	if n < 10 then 
	    [chr (ord '0' + toInt n)]
	else
	    let (q, r) = quotRem n 10 in
	    itos q ++ [chr (ord '0' + toInt r)]

chars :: Array Int Char
#if __HASKELL1__ < 3
chars = array (0,15) (zipWith (:=) [0..] "0123456789abcdef")
#else
chars = array (0,15) (zipWith (\x y -> (x,y)) [0..] "0123456789abcdef")
#endif

itosb :: Integer -> Integer -> String
itosb b n = 
	if n < b then 
	    [chars ! fromInteger n]
	else
	    let (q, r) = quotRem n b in
	    itosb b q ++ [chars ! fromInteger r]

stoi :: Int -> String -> (Int, String)
stoi a (c:cs) | isDigit c = stoi (a*10 + ord c - ord '0') cs
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
getSpecs l z cs@(c:_) us | isDigit c =
	let (n, cs') = stoi 0 cs
	    (p, cs'') = case cs' of
			'.':r -> stoi 0 r
			_     -> (-1, cs')
	in  (n, p, l, z, cs'', us)
getSpecs l z cs       us = (0, -1, l, z, cs, us)

#if !defined(__YALE_HASKELL__)
dfmt :: Char -> Int -> Double -> (String, String)
#endif

#if defined(__GLASGOW_HASKELL__)
dfmt c{-e,f, or g-} prec d
  = unsafePerformPrimIO (
	newCharArray (0 :: Int, 511){-pathetic malloc-} `thenStrictlyST` \ sprintf_here ->
	let
	    sprintf_fmt  = "%1" ++ (if prec < 0 then "" else '.':itos prec) ++ [c]
	in
	_ccall_ sprintf sprintf_here sprintf_fmt d  `seqPrimIO`
	freezeCharArray sprintf_here		    `thenST` \ (_ByteArray _ arr#) ->
	let
            unpack :: Int# -> [Char]
            unpack nh = case (ord# (indexCharArray# arr# nh)) of
		        0# -> []
		        ch -> case (nh +# 1#) of
			      mh -> C# (chr# ch) : unpack mh
        in
	returnPrimIO (
	case (indexCharArray# arr# 0#) of
	  '-'# -> ("-", unpack 1#)
	  _    -> ("" , unpack 0#)
  	)
    )
#endif

#if defined(__HBC__)
dfmt c p d = 
	case fmtf ("1" ++ (if p < 0 then "" else '.':itos p) ++ [c]) d of
	'-':cs -> ("-", cs)
	cs     -> ("" , cs)
#endif

#if defined(__YALE_HASKELL__)
fmte p d =
  case (primFmte p d) of
    '-':cs -> ("-",cs)
    cs     -> ("",cs)
fmtf p d =
  case (primFmtf p d) of
    '-':cs -> ("-",cs)
    cs     -> ("",cs)
fmtg p d =
  case (primFmtg p d) of
    '-':cs -> ("-",cs)
    cs     -> ("",cs)
#endif

perror s = error ("Printf.printf: "++s)
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"

#if defined(__YALE_HASKELL__)
-- This is needed because standard Haskell does not have toInt

toInt :: Integral a => a -> Int
toInt x = fromIntegral x
#endif
