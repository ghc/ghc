module PreludeCore (
	_integer_0,
	_integer_1,
	_integer_m1,
	fromRationalX,
	i0__,
	i1__,
	i2__,
	iminus1__,
	int2Integer,
	_showRational,
	r0__,
	r1__,
	rhalf__,
	_readList, _showList,
	_properFraction, _truncate, _round, _ceiling, _floor
    ) where

import Cls
import IChar	    -- instances
import IDouble
import IFloat
import IInt
import IInteger
import IList
import IRatio
import List		( reverse, dropWhile, take, drop, repeat, (++), head, tail )
import Prel		( (&&), (^^), (^), not, otherwise, asTypeOf, const, (.) )
import PS		( _PackedString, _unpackPS )
import Text
import TyComplex    -- for pragmas

-----------------------------------------------------------------
-- some *** NON-STANDARD *** constants (to help compiling Cls.hs)

i0__, iminus1__, i1__, i2__ :: Num a => a

{-# SPECIALIZE i0__ :: Int, Integer #-}

i0__		= fromInteger 0
iminus1__	= fromInteger (-1)
i1__		= fromInteger 1
i2__		= fromInteger 2

r0__, rhalf__, r1__ :: Fractional a => a

r0__		= fromRational 0
rhalf__		= fromRational 0.5
r1__		= fromRational 1

-- other bits of PreludeCore that aren't classes, instances, etc.

{- OLD:
absReal		:: (Real a) => a -> a
absReal x    | x >= 0	 =  x
	     | otherwise =  - x

signumReal	:: (Real a) => a -> a
signumReal x | x == 0	 =  0
	     | x > 0	 =  1
	     | otherwise = -1
-}

{- *RAW PRELUDE*: NOT REALLY USED:
numericEnumFrom		:: (Real a) => a -> [a]
numericEnumFromThen	:: (Real a) => a -> a -> [a]
numericEnumFrom		=  iterate (+1)
numericEnumFromThen n m	=  iterate (+(m-n)) n
-}

{- OLD:
realFloatToRational :: (RealFloat a) => a -> Rational
realFloatToRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
-}

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

--Joe
-}

--{-# GENERATE_SPECS rationalToRealFloat a{Double#,Double} #-}
rationalToRealFloat :: (RealFloat a) => Rational -> a

rationalToRealFloat x	=  x'
	where x'    = f e

--		If the exponent of the nearest floating-point number to x 
--		is e, then the significand is the integer nearest xb^(-e),
--		where b is the floating-point radix.  We start with a good
--		guess for e, and if it is correct, the exponent of the
--		floating-point number we construct will again be e.  If
--		not, one more iteration is needed.

	      f e   = if e' == e then y else f e'
		      where y	   = encodeFloat (round (x * (1%b)^^e)) e
			    (_,e') = decodeFloat y
	      b	    = floatRadix x'

--		We obtain a trial exponent by doing a floating-point
--		division of x's numerator by its denominator.  The
--		result of this division may not itself be the ultimate
--		result, because of an accumulation of three rounding
--		errors.

	      (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
					/ fromInteger (denominator x))

-------------------------------------------------------------------------
-- These RealFrac things are here so we can
-- SPECIALIZE the tapookies out of them.
-- Also: get rid of gratuitous lazy pattern matching.

_properFraction	    :: (RealFloat a, Integral b) => a -> (b,a)
_truncate, _round   :: (RealFrac a, Integral b) => a -> b
_ceiling, _floor    :: (RealFrac a, Integral b) => a -> b

{-# SPECIALIZE _properFraction
	:: Double -> (Int, Double)
  #-}
{-# SPECIALIZE _truncate
	:: Double -> Int
  #-}
{-# SPECIALIZE _round
	:: Double   -> Int,
	   Rational -> Integer
  #-}
{-# SPECIALIZE _ceiling
	:: Double -> Int
  #-}
{-# SPECIALIZE _floor
	:: Double -> Int
  #-}

_properFraction x
  = case (decodeFloat x)      of { (m,n) ->
    let  b = floatRadix x     in
    if n >= 0 then
	(fromInteger m * fromInteger b ^ n, 0)
    else
	case (quotRem m (b^(-n))) of { (w,r) ->
	(fromInteger w, encodeFloat r n)
	}
    }

_truncate x =  case (properFraction x) of { (m, _) -> m }

_round x
  -- this defn differs from that in the report; uses _tagCmp
  --
  = case (properFraction x) of { (n,r) ->
    let
	m     = if r < r0__ then n - i1__ else n + i1__
	sign  = signum (abs r - rhalf__) --UNUSED!

	half_down = abs r - rhalf__
    in
    case (_tagCmp half_down r0__) of
      _LT -> n
      _EQ -> if even n then n else m
      _GT -> m
{- OLD:
    if sign == iminus1__ then n
    else if sign == i0__ then (if even n then n else m)
    else if sign == i1__ then m
    else error "_round{PreludeCore}: no match in sign\n"
-}
    }

_ceiling x
  = case (properFraction x) of { (n,r) ->
    if r > r0__ then n + i1__ else n }

_floor x
  = case (properFraction x) of { (n,r) ->
    if r < r0__ then n - i1__ else n }

-------------------------------------------------------------------------
-- from/by Lennart, 94/09/26

--module Rational(prRational, fromRationalX, tinyDouble, tinyFloat, hugeDouble, hugeFloat, tiny, huge, integerLogBase) where

-- Convert a Rational to a string that looks like a floating point number,
-- but without converting to any floating type (because of the possible overflow).
_showRational :: Int -> Rational -> String
_showRational n r =
    if r == 0 then
    	"0.0"
    else
	let (r', e) = normalize r
	in  prR n r' e

startExpExp = 4 :: Int

-- make sure 1 <= r < 10
normalize :: Rational -> (Rational, Int)
normalize r = if r < 1 then case norm startExpExp (1 / r) 0 of (r', e) -> (10 / r', -e-1) else norm startExpExp r 0
	where norm :: Int -> Rational -> Int -> (Rational, Int)
	      -- Invariant: r*10^e == original r
	      norm 0  r e = (r, e)
	      norm ee r e =
		let n = 10^ee
		    tn = 10^n
		in  if r >= tn then norm ee (r/tn) (e+n) else norm (ee-1) r e

drop0 "" = ""
drop0 (c:cs) = c : reverse (dropWhile (=='0') (reverse cs))

prR :: Int -> Rational -> Int -> String
prR n r e | r <   1 = prR n (r*10) (e-1)		-- final adjustment
prR n r e | r >= 10 = prR n (r/10) (e+1)
prR n r e0 =
	let s = show ((_round (r * 10^n))::Integer)
	    e = e0+1
	in  if e > 0 && e < 8 then
		take e s ++ "." ++ drop0 (drop e s)
	    else if e <= 0 && e > -3 then
	        "0." ++ take (-e) (repeat '0') ++ drop0 s
	    else
	        head s : "."++ drop0 (tail s) ++ "e" ++ show e0

------------

-- The Prelude version of fromRational is broken; if the denominator or nominator is
-- out of range it fails.  So we use this (very expensive!) version instead.

fromRationalX :: (RealFloat a) => Rational -> a

fromRationalX r =
  rationalToRealFloat r
{- Hmmm...
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
-}

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

tinyDouble = tiny :: Double
tinyFloat  = tiny :: Float
hugeDouble = huge :: Double
hugeFloat  = huge :: Float

-----------------------------------------------------------------
-- It is problematic having this in Cls.hs
-- (You really don't want to know why -- WDP 94/12)
--
_readList :: Text a => ReadS [a]

_readList   = readParen False (\r -> [pr | ("[",s)  <- lex r,
					   pr	    <- readl s])
	      where readl  s = [([],t)   | ("]",t)  <- lex s] ++
			       [(x:xs,u) | (x,t)    <- reads s,
					   (xs,u)   <- readl2 t]
		    readl2 s = [([],t)   | ("]",t)  <- lex s] ++
			       [(x:xs,v) | (",",t)  <- lex s,
					   (x,u)    <- reads t,
					   (xs,v)   <- readl2 u]

_showList :: Text a => [a] -> ShowS

_showList [] = showString "[]"
_showList (x:xs)
	     = showChar '[' . shows x . showl xs

	       where showl []     = showChar ']'
		     showl (x:xs) = showString ", " . shows x . showl xs
