module PreludeCore (
	__i0,
	__i1,
	__i2,
	__im1,
	__i8,
	__i10,
	__i16,
	__rhalf,
	_fromRational,
	_showRational,
	_readList,
	_showList
    ) where

import Cls
import IChar
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import IList
import IRatio
import List		( reverse, dropWhile, take, drop, repeat, (++), head, tail )
import Prel		( (&&), (^^), (^), not, otherwise, asTypeOf, const, (.), atan2, maxInt )
import PS		( _PackedString, _unpackPS )
import Text
import TyComplex
import TyArray

-----------------------------------------------------------------
-- some *** NON-STANDARD *** constants (to help compiling Cls.hs)


{-# GENERATE_SPECS __i0 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i0   	:: Num a => a
{-# GENERATE_SPECS __i1 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i1  :: Num a => a
{-# GENERATE_SPECS __i2 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i2  :: Num a => a
{-# GENERATE_SPECS __im1 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__im1 :: Num a => a
{-# GENERATE_SPECS __i8 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i8  :: Num a => a
{-# GENERATE_SPECS __i10 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i10 :: Num a => a
{-# GENERATE_SPECS __i16 a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double),Rational} #-}
__i16 :: Num a => a

__i0	= fromInt 0
__i1	= fromInt 1
__i2	= fromInt 2
__im1	= fromInt (-1)
__i8	= fromInt 8
__i10	= fromInt 10
__i16	= fromInt 16

{-# GENERATE_SPECS __rhalf a{Double#,Double,Complex(Double#),Complex(Double),Rational} #-}
__rhalf :: Fractional a => a
__rhalf	= fromRational (__i1:%__i2)


-- bits of PreludeCore that aren't classes, instances, etc.

{-
[In response to a request by simonpj, Joe Fasel writes:]

A quite reasonable request!  This code was added to the Prelude just
before the 1.2 release, when Lennart, working with an early version
of hbi, noticed that (read . show) was not the identity for
floating-point numbers.	 (There was a one-bit error about half the time.)
The original version of the conversion function was in fact simply
a floating-point divide, as you suggest above.	The new version is,
I grant you, somewhat denser.

How's this?

Joe
-}

{-# GENERATE_SPECS _fromRational a{Double#,Double} #-}
_fromRational :: (RealFloat a) => Rational -> a
_fromRational x = x'
	where x' = f e

--		If the exponent of the nearest floating-point number to x 
--		is e, then the significand is the integer nearest xb^(-e),
--		where b is the floating-point radix.  We start with a good
--		guess for e, and if it is correct, the exponent of the
--		floating-point number we construct will again be e.  If
--		not, one more iteration is needed.

	      f e   = if e' == e then y else f e'
		      where y	   = encodeFloat (round (x * (__i1 % b)^^e)) e
			    (_,e') = decodeFloat y
	      b	    = floatRadix x'

--		We obtain a trial exponent by doing a floating-point
--		division of x's numerator by its denominator.  The
--		result of this division may not itself be the ultimate
--		result, because of an accumulation of three rounding
--		errors.

	      (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
					/ fromInteger (denominator x))


{- Hmmm... 

-- Another version of _fromRational which is floating around ...
-- Any idea what is the true story ? (PS)

_fromRational :: (RealFloat a) => Rational -> a
_fromRational r
	let 
	    h = ceiling (huge `asTypeOf` x)
	    b = toInteger (floatRadix x)
	    x = fromRat 0 r

	    fromRat e0 r' =
{--}		_trace (shows e0 ('/' : shows r' ('/' : shows h "\n"))) (
		let d = denominator r'
		    n = numerator r'
	        in  if d > h then
		       let e = integerLogBase b (d `div` h) + 1
		       in  fromRat (e0-e) (n % (d `div` (b^e)))
		    else if abs n > h then
		       let e = integerLogBase b (abs n `div` h) + 1
		       in  fromRat (e0+e) ((n `div` (b^e)) % d)
		    else
		       scaleFloat e0 (rationalToRealFloat r')
		       -- now that we know things are in-bounds,
		       -- we use the "old" Prelude code.
{--}		)
	in  x

-- Compute the discrete log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
	-- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
	    doDiv :: Integer -> Int -> Int
	    doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
	in  doDiv (i `div` (b^l)) l


------------

-- Compute smallest and largest floating point values.
tiny :: (RealFloat a) => a
tiny =
	let (l, _) = floatRange x
	    x = encodeFloat 1 (l-1)
	in  x

huge :: (RealFloat a) => a
huge =
	let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in  x

...mmmH -}

-------------------------------------------------------------------------
-- from/by Lennart, 94/09/26

-- Convert a Rational to a string that looks like a floating point number,
-- but without converting to any floating type (because of the possible overflow).
_showRational :: Int -> Rational -> String
_showRational n r =
    if r == __i0 then
    	"0.0"
    else
	let (r', e) = normalize r
	in  prR n r' e

startExpExp = 4 :: Int

-- make sure 1 <= r < 10
normalize :: Rational -> (Rational, Int)
normalize r = if r < __i1 then
		  case norm startExpExp (__i1 / r) 0 of (r', e) -> (__i10 / r', -e-1)
	      else
		  norm startExpExp r 0
	where norm :: Int -> Rational -> Int -> (Rational, Int)
	      -- Invariant: r*10^e == original r
	      norm 0  r e = (r, e)
	      norm ee r e =
		let n = 10^ee
		    tn = __i10^n
		in  if r >= tn then norm ee (r/tn) (e+n) else norm (ee-1) r e

drop0 "" = ""
drop0 (c:cs) = c : reverse (dropWhile (=='0') (reverse cs))

prR :: Int -> Rational -> Int -> String
prR n r e | r <  __i1  = prR n (r*__i10) (e-1)		-- final adjustment
prR n r e | r >= __i10 = prR n (r/__i10) (e+1)
prR n r e0 =
	let s = show ((round (r * __i10^n))::Integer)
	    e = e0+1
	in  if e > 0 && e < 8 then
		take e s ++ "." ++ drop0 (drop e s)
	    else if e <= 0 && e > -3 then
	        "0." ++ take (-e) (repeat '0') ++ drop0 s
	    else
	        head s : "."++ drop0 (tail s) ++ "e" ++ show e0

-----------------------------------------------------------------

{-# GENERATE_SPECS _readList a #-}
_readList :: ReadS a -> ReadS [a]

_readList readx = readParen False (\r -> [pr | ("[",s)  <- lex r,
					       pr       <- readl s])
	          where readl  s = [([],t)   | ("]",t)  <- lex s] ++
			           [(x:xs,u) | (x,t)    <- readx s,
					       (xs,u)   <- readl2 t]
		        readl2 s = [([],t)   | ("]",t)  <- lex s] ++
			           [(x:xs,v) | (",",t)  <- lex s,
					       (x,u)    <- readx t,
					       (xs,v)   <- readl2 u]

{-# GENERATE_SPECS _showList a #-}
_showList :: (a -> ShowS) ->  [a] -> ShowS

_showList showx [] = showString "[]"
_showList showx (x:xs)
	     = showChar '[' . showx x . showl xs

	       where showl []     = showChar ']'
		     showl (x:xs) = showString ", " . showx x . showl xs
