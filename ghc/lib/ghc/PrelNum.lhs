%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelNum]{Module @PrelNum@}

Numeric part of the prelude.

It's rather big!

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/floatExtreme.h" #-}
{-# OPTIONS -H20m #-}

#include "../includes/ieee-flpt.h"

\end{code}

\begin{code}
module PrelNum where

import PrelBase
import GHC
import {-# SOURCE #-}	IOBase	( error )
import PrelList

import ArrBase	( Array, array, (!) )
import UnsafeST ( unsafePerformPrimIO )
import Ix	( Ix(..) )
import Foreign	()		-- This import tells the dependency analyser to compile Foreign first.
				-- There's an implicit dependency on Foreign because the ccalls in
				-- PrelNum implicitly mention CCallable.

infixr 8  ^, ^^, **
infixl 7  /, %, `quot`, `rem`, `div`, `mod`
\end{code}


%*********************************************************
%*							*
\subsection{Standard numeric classes}
%*							*
%*********************************************************

\begin{code}
class  (Num a, Ord a) => Real a  where
    toRational		::  a -> Rational

class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    toInteger		:: a -> Integer
    toInt		:: a -> Int -- partain: Glasgow extension

    n `quot` d		=  q  where (q,r) = quotRem n d
    n `rem` d		=  r  where (q,r) = quotRem n d
    n `div` d		=  q  where (q,r) = divMod n d
    n `mod` d		=  r  where (q,r) = divMod n d
    divMod n d 		=  if signum r == negate (signum d) then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a

    recip x		=  1 / x

class  (Fractional a) => Floating a  where
    pi			:: a
    exp, log, sqrt	:: a -> a
    (**), logBase	:: a -> a -> a
    sin, cos, tan	:: a -> a
    asin, acos, atan	:: a -> a
    sinh, cosh, tanh	:: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y		=  exp (log x * y)
    logBase x y		=  log y / log x
    sqrt x		=  x ** 0.5
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b,a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b

    truncate x		=  m  where (m,_) = properFraction x
    
    round x		=  let (n,r) = properFraction x
    			       m     = if r < 0 then n - 1 else n + 1
    			   in case signum (abs r - 0.5) of
    				-1 -> n
    			 	0  -> if even n then n else m
    				1  -> m
    
    ceiling x		=  if r > 0 then n + 1 else n
    			   where (n,r) = properFraction x
    
    floor x		=  if r < 0 then n - 1 else n
    			   where (n,r) = properFraction x

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)
    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                        :: a -> Bool

    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (negate (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x
\end{code}

%*********************************************************
%*							*
\subsection{Overloaded numeric functions}
%*							*
%*********************************************************

\begin{code}
even, odd	:: (Integral a) => a -> Bool
even n		=  n `rem` 2 == 0
odd		=  not . even

{-# GENERATE_SPECS gcd a{Int#,Int,Integer} #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x 0  =  x
			 gcd' x y  =  gcd' y (x `rem` y)

{-# GENERATE_SPECS lcm a{Int#,Int,Integer} #-}
lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ n | n > 0	=  f x (n-1) x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | even n  = g (x*x) (n `quot` 2)
				         | otherwise = f x (n-1) (x*y)
_ ^ _		= error "Prelude.^: negative exponent"

(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(negate n))

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational

atan2		:: (RealFloat a) => a -> a -> a
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0
			( 1, 0) ->  pi/2
			( 0,-1) ->  pi
			(-1, 0) ->  (negate pi)/2
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			( 0, 0) ->  error "Prelude.atan2: atan2 of origin"
\end{code}


%*********************************************************
%*							*
\subsection{Instances for @Int@}
%*							*
%*********************************************************

\begin{code}
instance  Real Int  where
    toRational x	=  toInteger x % 1

instance  Integral Int	where
    a@(I# _) `quotRem` b@(I# _)	= (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

    -- following chks for zero divisor are non-standard (WDP)
    a `quot` b		=  if b /= 0
			   then a `quotInt` b
			   else error "Integral.Int.quot{PreludeCore}: divide by 0\n"
    a `rem` b		=  if b /= 0
			   then a `remInt` b
			   else error "Integral.Int.rem{PreludeCore}: divide by 0\n"

    x `div` y = if x > 0 && y < 0	then quotInt (x-y-1) y
		else if x < 0 && y > 0	then quotInt (x-y+1) y
		else quotInt x y
    x `mod` y = if x > 0 && y < 0 || x < 0 && y > 0 then
		    if r/=0 then r+y else 0
	    	else
		    r
	      where r = remInt x y

    divMod x@(I# _) y@(I# _) = (x `div` y, x `mod` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)

--OLD:   even x = eqInt (x `mod` 2) 0
--OLD:   odd x  = neInt (x `mod` 2) 0

    toInteger (I# n#) = int2Integer# n#  -- give back a full-blown Integer
    toInt x	      = x

\end{code}


%*********************************************************
%*							*
\subsection{Type @Integer@}
%*							*
%*********************************************************

These types are used to return from integer primops

\begin{code}
data Return2GMPs     = Return2GMPs     Int# Int# ByteArray# Int# Int# ByteArray#
data ReturnIntAndGMP = ReturnIntAndGMP Int# Int# Int# ByteArray#
\end{code}

Instances

\begin{code}
instance  Eq Integer  where
    (J# a1 s1 d1) == (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ==# 0#

    (J# a1 s1 d1) /= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) /=# 0#

instance  Ord Integer  where
    (J# a1 s1 d1) <= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <=# 0#

    (J# a1 s1 d1) <  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#

    (J# a1 s1 d1) >= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#

    (J# a1 s1 d1) >  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#

    x@(J# a1 s1 d1) `max` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#) then x else y

    x@(J# a1 s1 d1) `min` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#) then x else y

    compare (J# a1 s1 d1) (J# a2 s2 d2)
       = case cmpInteger# a1 s1 d1 a2 s2 d2 of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }

instance  Num Integer  where
    (+) (J# a1 s1 d1) (J# a2 s2 d2)
      = plusInteger# a1 s1 d1 a2 s2 d2

    (-) (J# a1 s1 d1) (J# a2 s2 d2)
      = minusInteger# a1 s1 d1 a2 s2 d2

    negate (J# a s d) = negateInteger# a s d

    (*) (J# a1 s1 d1) (J# a2 s2 d2)
      = timesInteger# a1 s1 d1 a2 s2 d2

    -- ORIG: abs n = if n >= 0 then n else -n

    abs n@(J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	if (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#
	then n
	else negateInteger# a1 s1 d1
	}

    signum n@(J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	let
	    cmp = cmpInteger# a1 s1 d1 a2 s2 d2
	in
	if      cmp >#  0# then 1
	else if cmp ==# 0# then 0
	else			(negate 1)
	}

    fromInteger	x	=  x

    fromInt (I# n#)	=  int2Integer# n# -- gives back a full-blown Integer

instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
    quotRem (J# a1 s1 d1) (J# a2 s2 d2)
      = case (quotRemInteger# a1 s1 d1 a2 s2 d2) of
	  Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)

{- USING THE UNDERLYING "GMP" CODE IS DUBIOUS FOR NOW:

    divMod (J# a1 s1 d1) (J# a2 s2 d2)
      = case (divModInteger# a1 s1 d1 a2 s2 d2) of
	  Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)
-}
    toInteger n	     = n
    toInt (J# a s d) = case (integer2Int# a s d) of { n# -> I# n# }

    -- the rest are identical to the report default methods;
    -- you get slightly better code if you let the compiler
    -- see them right here:
    n `quot` d	=  q  where (q,r) = quotRem n d
    n `rem` d	=  r  where (q,r) = quotRem n d
    n `div` d	=  q  where (q,r) = divMod n d
    n `mod` d	=  r  where (q,r) = divMod n d

    divMod n d 	=  case (quotRem n d) of { qr@(q,r) ->
		   if signum r == negate (signum d) then (q - 1, r+d) else qr }
		   -- Case-ified by WDP 94/10

instance  Enum Integer  where
    toEnum n		 =  toInteger n
    fromEnum n		 =  toInt n
    enumFrom n           =  n : enumFrom (n + 1)
    enumFromThen m n     =  en' m (n - m)
	                    where en' m n = m : en' (m + n) n
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Show Integer  where
    showsPrec   x = showSignedInteger x
    showList = showList__ (showsPrec 0) 

instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  fromInteger (i - m)
	| otherwise	=  error "Integer.index: Index out of range."
    inRange (m,n) i	=  m <= i && i <= n

integer_0, integer_1, integer_2, integer_m1 :: Integer
integer_0  = int2Integer# 0#
integer_1  = int2Integer# 1#
integer_2  = int2Integer# 2#
integer_m1 = int2Integer# (negateInt# 1#)
\end{code}


%*********************************************************
%*							*
\subsection{Type @Float@}
%*							*
%*********************************************************

\begin{code}
instance Eq Float where
    (F# x) == (F# y) = x `eqFloat#` y

instance Ord Float where
    (F# x) `compare` (F# y) | x `ltFloat#` y = LT
			    | x `eqFloat#` y = EQ
			    | otherwise      = GT

    (F# x) <  (F# y) = x `ltFloat#`  y
    (F# x) <= (F# y) = x `leFloat#`  y
    (F# x) >= (F# y) = x `geFloat#`  y
    (F# x) >  (F# y) = x `gtFloat#`  y

instance  Num Float  where
    (+)		x y 	=  plusFloat x y
    (-)		x y 	=  minusFloat x y
    negate	x  	=  negateFloat x
    (*)		x y 	=  timesFloat x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateFloat x
    signum x | x == 0.0	 = 0
	     | x > 0.0	 = 1
	     | otherwise = negate 1
    fromInteger n	=  encodeFloat n 0
    fromInt i		=  int2Float i

instance  Real Float  where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Float  where
    (/) x y		=  divideFloat x y
    fromRational x	=  fromRat x
    recip x		=  1.0 / x

instance  Floating Float  where
    pi			=  3.141592653589793238
    exp x		=  expFloat x
    log	x	 	=  logFloat x
    sqrt x		=  sqrtFloat x
    sin	x		=  sinFloat x
    cos	x		=  cosFloat x
    tan	x		=  tanFloat x
    asin x		=  asinFloat x
    acos x		=  acosFloat x
    atan x		=  atanFloat x
    sinh x		=  sinhFloat x
    cosh x	 	=  coshFloat x
    tanh x		=  tanhFloat x
    (**) x y		=  powerFloat x y
    logBase x y		=  log y / log x

    asinh x = log (x + sqrt (1.0+x*x))
    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    atanh x = log ((x+1.0) / sqrt (1.0-x*x))

instance  RealFrac Float  where

    {-# SPECIALIZE properFraction :: Float -> (Int, Float) #-}
    {-# SPECIALIZE truncate :: Float -> Int #-}
    {-# SPECIALIZE round    :: Float -> Int #-}
    {-# SPECIALIZE ceiling  :: Float -> Int #-}
    {-# SPECIALIZE floor    :: Float -> Int #-}

    {-# SPECIALIZE properFraction :: Float -> (Integer, Float) #-}
    {-# SPECIALIZE truncate :: Float -> Integer #-}
    {-# SPECIALIZE round    :: Float -> Integer #-}
    {-# SPECIALIZE ceiling  :: Float -> Integer #-}
    {-# SPECIALIZE floor    :: Float -> Integer #-}

    properFraction x
      = case (decodeFloat x)      of { (m,n) ->
    	let  b = floatRadix x     in
    	if n >= 0 then
	    (fromInteger m * fromInteger b ^ n, 0.0)
    	else
	    case (quotRem m (b^(negate n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < 0.0 then n - 1 else n + 1
		  	      	half_down = abs r - 0.5
    		   	      in
    		   	      case (compare half_down 0.0) of
      		     		LT -> n
      		     		EQ -> if even n then n else m
      		     		GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > 0.0 then n + 1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < 0.0 then n - 1 else n

instance  RealFloat Float  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  FLT_MANT_DIG	    -- ditto
    floatRange _	=  (FLT_MIN_EXP, FLT_MAX_EXP) -- ditto

    decodeFloat (F# f#)
      = case decodeFloat# f#	of
	  ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeFloat# a# s# d# e# of { flt# -> F# flt# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)
    isNaN x = 
      (0::Int) /= unsafePerformPrimIO (_ccall_ isFloatNaN x) {- a _pure_function! -}
    isInfinite x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isFloatInfinite x) {- ditto! -}
    isDenormalized x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isFloatDenormalized x) -- ..
    isNegativeZero x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isFloatNegativeZero x) -- ...
    isIEEE x    = True

instance  Show Float  where
    showsPrec   x = showSigned showFloat x
    showList = showList__ (showsPrec 0) 
\end{code}

%*********************************************************
%*							*
\subsection{Type @Double@}
%*							*
%*********************************************************

\begin{code}
instance Eq Double where
    (D# x) == (D# y) = x ==## y

instance Ord Double where
    (D# x) `compare` (D# y) | x <## y   = LT
			    | x ==## y  = EQ
			    | otherwise = GT

    (D# x) <  (D# y) = x <##  y
    (D# x) <= (D# y) = x <=## y
    (D# x) >= (D# y) = x >=## y
    (D# x) >  (D# y) = x >##  y

instance  Num Double  where
    (+)		x y 	=  plusDouble x y
    (-)		x y 	=  minusDouble x y
    negate	x  	=  negateDouble x
    (*)		x y 	=  timesDouble x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateDouble x
    signum x | x == 0.0	 = 0
	     | x > 0.0	 = 1
	     | otherwise = negate 1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  case (int2Double# n#) of { d# -> D# d# }

instance  Real Double  where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double  where
    (/) x y		=  divideDouble x y
    fromRational x	=  fromRat x
    recip x		=  1.0 / x

instance  Floating Double  where
    pi			=  3.141592653589793238
    exp	x		=  expDouble x
    log	x		=  logDouble x
    sqrt x		=  sqrtDouble x
    sin	 x		=  sinDouble x
    cos	 x		=  cosDouble x
    tan	 x		=  tanDouble x
    asin x		=  asinDouble x
    acos x	 	=  acosDouble x
    atan x		=  atanDouble x
    sinh x		=  sinhDouble x
    cosh x		=  coshDouble x
    tanh x		=  tanhDouble x
    (**) x y		=  powerDouble x y
    logBase x y		=  log y / log x

    asinh x = log (x + sqrt (1.0+x*x))
    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    atanh x = log ((x+1.0) / sqrt (1.0-x*x))

instance  RealFrac Double  where

    {-# SPECIALIZE properFraction :: Double -> (Int, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Int #-}
    {-# SPECIALIZE round    :: Double -> Int #-}
    {-# SPECIALIZE ceiling  :: Double -> Int #-}
    {-# SPECIALIZE floor    :: Double -> Int #-}

    {-# SPECIALIZE properFraction :: Double -> (Integer, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Integer #-}
    {-# SPECIALIZE round    :: Double -> Integer #-}
    {-# SPECIALIZE ceiling  :: Double -> Integer #-}
    {-# SPECIALIZE floor    :: Double -> Integer #-}

#if defined(__UNBOXED_INSTANCES__)
    {-# SPECIALIZE properFraction :: Double -> (Int#, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Int# #-}
    {-# SPECIALIZE round    :: Double -> Int# #-}
    {-# SPECIALIZE ceiling  :: Double -> Int# #-}
    {-# SPECIALIZE floor    :: Double -> Int# #-}
#endif

    properFraction x
      = case (decodeFloat x)      of { (m,n) ->
    	let  b = floatRadix x     in
    	if n >= 0 then
	    (fromInteger m * fromInteger b ^ n, 0.0)
    	else
	    case (quotRem m (b^(negate n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < 0.0 then n - 1 else n + 1
		  	      	half_down = abs r - 0.5
    		   	      in
    		   	      case (compare half_down 0.0) of
      		     		LT -> n
      		     		EQ -> if even n then n else m
      		     		GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > 0.0 then n + 1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < 0.0 then n - 1 else n

instance  RealFloat Double  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  DBL_MANT_DIG	    -- ditto
    floatRange _	=  (DBL_MIN_EXP, DBL_MAX_EXP) -- ditto

    decodeFloat (D# d#)
      = case decodeDouble# d#	of
	  ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeDouble# a# s# d# e#	of { dbl# -> D# dbl# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)
    isNaN x = 
      (0::Int) /= unsafePerformPrimIO (_ccall_ isDoubleNaN x) {- a _pure_function! -}
    isInfinite x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isDoubleInfinite x) {- ditto -}
    isDenormalized x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isDoubleDenormalized x) -- ..
    isNegativeZero x =
      (0::Int) /= unsafePerformPrimIO (_ccall_ isDoubleNegativeZero x) -- ...
    isIEEE x    = True

instance  Show Double  where
    showsPrec   x = showSigned showFloat x
    showList = showList__ (showsPrec 0) 
\end{code}


%*********************************************************
%*							*
\subsection{Common code for @Float@ and @Double@}
%*							*
%*********************************************************

The @Enum@ instances for Floats and Doubles are slightly unusual.
The @toEnum@ function truncates numbers to Int.  The definitions
of @enumFrom@ and @enumFromThen@ allow floats to be used in arithmetic
series: [0,0.1 .. 1.0].  However, roundoff errors make these somewhat
dubious.  This example may have either 10 or 11 elements, depending on
how 0.1 is represented.

NOTE: The instances for Float and Double do not make use of the default
methods for @enumFromTo@ and @enumFromThenTo@, as these rely on there being
a `non-lossy' conversion to and from Ints. Instead we make use of the 
1.2 default methods (back in the days when Enum had Ord as a superclass)
for these (@numericEnumFromTo@ and @numericEnumFromThenTo@ below.)

\begin{code}
instance  Enum Float  where
    toEnum         =  fromIntegral
    fromEnum       =  fromInteger . truncate   -- may overflow
    enumFrom	   =  numericEnumFrom
    enumFromThen   =  numericEnumFromThen
    enumFromThenTo =  numericEnumFromThenTo

instance  Enum Double  where
    toEnum         =  fromIntegral
    fromEnum       =  fromInteger . truncate   -- may overflow
    enumFrom	   =  numericEnumFrom
    enumFromThen   =  numericEnumFromThen
    enumFromThenTo =  numericEnumFromThenTo

numericEnumFrom		:: (Real a) => a -> [a]
numericEnumFromThen	:: (Real a) => a -> a -> [a]
numericEnumFromThenTo   :: (Real a) => a -> a -> a -> [a]
numericEnumFrom		=  iterate (+1)
numericEnumFromThen n m	=  iterate (+(m-n)) n
numericEnumFromThenTo n m p = takeWhile (if m >= n then (<= p) else (>= p))
				      (numericEnumFromThen n m)
\end{code}


%*********************************************************
%*							*
\subsection{The @Ratio@ and @Rational@ types}
%*							*
%*********************************************************

\begin{code}
data  (Integral a)	=> Ratio a = !a :% !a  deriving (Eq)
type  Rational		=  Ratio Integer
\end{code}

\begin{code}
(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a
approxRational		:: (RealFrac a) => a -> a -> Rational

\end{code}

\tr{reduce} is a subsidiary function used only in this module .
It normalises a ratio by dividing both numerator and denominator by
their greatest common divisor.

\begin{code}
reduce x 0		=  error "{Ratio.%}: zero denominator"
reduce x y		=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y
\end{code}

\begin{code}
x % y			=  reduce (x * signum y) (abs y)

numerator (x:%y)	=  x

denominator (x:%y)	=  y
\end{code}


@approxRational@, applied to two real fractional numbers x and epsilon,
returns the simplest rational number within epsilon of x.  A rational
number n%d in reduced form is said to be simpler than another n'%d' if
abs n <= abs n' && d <= d'.  Any real interval contains a unique
simplest rational; here, for simplicity, we assume a closed rational
interval.  If such an interval includes at least one whole number, then
the simplest rational is the absolutely least whole number.  Otherwise,
the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
and abs r' < d', and the simplest rational is q%1 + the reciprocal of
the simplest rational between d'%r' and d%r.

\begin{code}
approxRational x eps	=  simplest (x-eps) (x+eps)
	where simplest x y | y < x	=  simplest y x
			   | x == y	=  xr
			   | x > 0	=  simplest' n d n' d'
			   | y < 0	=  - simplest' (-n') d' (-n) d
			   | otherwise	=  0 :% 1
					where xr  = toRational x
					      n   = numerator xr
					      d   = denominator xr
					      nd' = toRational y
					      n'  = numerator nd'
					      d'  = denominator nd'

	      simplest' n d n' d'	-- assumes 0 < n%d < n'%d'
			| r == 0     =	q :% 1
			| q /= q'    =	(q+1) :% 1
			| otherwise  =	(q*n''+d'') :% n''
				     where (q,r)      =	 quotRem n d
					   (q',r')    =	 quotRem n' d'
					   nd''       =  simplest' d' r' d r
					   n''        =  numerator nd''
					   d''        =	 denominator nd''
\end{code}


\begin{code}
instance  (Integral a)	=> Ord (Ratio a)  where
    (x:%y) <= (x':%y')	=  x * y' <= x' * y
    (x:%y) <  (x':%y')	=  x * y' <  x' * y

instance  (Integral a)	=> Num (Ratio a)  where
    (x:%y) + (x':%y')	=  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')	=  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

instance  (Integral a)	=> Enum (Ratio a)  where
    enumFrom		=  iterate ((+)1)
    enumFromThen n m	=  iterate ((+)(m-n)) n
    toEnum n            =  fromIntegral n :% 1
    fromEnum            =  fromInteger . truncate

ratio_prec :: Int
ratio_prec = 7

instance  (Integral a)  => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > ratio_prec)
    	    	    	       (shows x . showString " % " . shows y)
\end{code}

\begin{code}
--Exported from std library Numeric, defined here to
--avoid mut. rec. between PrelNum and Numeric.
showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

showSignedInteger :: Int -> Integer -> ShowS
showSignedInteger p n r
  = -- from HBC version; support code follows
    if n < 0 && p > 6 then '(':jtos n++(')':r) else jtos n ++ r

jtos :: Integer -> String
jtos n 
  = if n < 0 then
        '-' : jtos' (-n) []
    else 
	jtos' n []

jtos' :: Integer -> String -> String
jtos' n cs
  = if n < 10 then
	chr (fromInteger (n + ord_0)) : cs
    else 
	jtos' (n `quot` 10) (chr (fromInteger (n `rem` 10 + ord_0)) : cs)

showFloat x  =  showString (formatRealFloat FFGeneric Nothing x)

-- These are the format types.  This type is not exported.

data FFFormat = FFExponent | FFFixed | FFGeneric --no need: deriving (Eq, Ord, Show)

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = s
 where 
  base = 10
  s = if isNaN x 
      then "NaN"
      else 
       if isInfinite x then
          if x < 0 then "-Infinity" else "Infinity"
       else
          if x < 0 || isNegativeZero x then
            '-':doFmt fmt (floatToDigits (toInteger base) (-x))
          else
	    doFmt fmt (floatToDigits (toInteger base) x)

  doFmt fmt (is, e) =
    let ds = map intToDigit is in
    case fmt of
     FFGeneric ->
      doFmt (if e <0 || e > 7 then FFExponent else FFFixed)
	    (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let e' = if e==0 then 0 else e-1 in
	(case ds of
          [d]    -> d : ".0e"
	  (d:ds) -> d : '.' : ds ++ "e") ++ show e'
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0':'.':take dec' (repeat '0') ++ "e0"
         _ ->
          let
	   (ei,is') = roundTo base (dec'+1) is
	   d:ds = map intToDigit (if ei > 0 then init is' else is')
          in
	  d:'.':ds ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing ->
         let
	  f 0 s ds = mk0 (reverse s) ++ '.':mk0 ds
	  f n s "" = f (n-1) ('0':s) ""
	  f n s (d:ds) = f (n-1) (d:s) ds
	 in
	 f e "" ds
       Just dec ->
        let dec' = max dec 1 in
	if e >= 0 then
	 let
	  (ei,is') = roundTo base (dec' + e) is
	  (ls,rs)  = splitAt (e+ei) (map intToDigit is')
	 in
	 mk0 ls ++ (if null rs then "" else '.':rs)
	else
	 let
	  (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
	  d:ds = map intToDigit (if ei > 0 then is' else 0:is')
	 in
	 d : '.' : ds
	 

roundTo :: Int -> Int -> [Int] -> (Int,[Int])
roundTo base d is =
 let
  v = f d is
 in
 case v of
  (0,is) -> v
  (1,is) -> (1, 1:is)
 where
  b2 = base `div` 2

  f n [] = (0, replicate n 0)
  f 0 (i:_) = (if i>=b2 then 1 else 0, [])
  f d (i:is) =
    let 
     (c,ds) = f (d-1) is
     i' = c + i
    in
    if i' == base then (1,0:ds) else (0,i':ds)

--
-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R.K. Dybvig in PLDI 96.
-- This version uses a much slower logarithm estimator. It should be improved.

-- This function returns a list of digits (Ints in [0..base-1]) and an
-- exponent.
--floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits _ 0 = ([0], 0)
floatToDigits base x =
 let 
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) = 
   let n = minExp - e0 in
   if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = b^ e in
    if f == b^(p-1) then
      (f*be*b*2, 2*b, be*b, b)
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == b^(p-1) then
      (f*b*2, b^(-e+1)*2, b, 1)
    else
      (f*2, b^(-e)*2, 1, 1)
  k =
   let 
    k0 =
     if b == 2 && base == 10 then
        -- logBase 10 2 is slightly bigger than 3/10 so
	-- the following will err on the low side.  Ignoring
	-- the fraction will make it err even more.
	-- Haskell promises that p-1 <= logBase b f < p.
	(p - 1 + e0) * 3 `div` 10
     else
        ceiling ((log (fromInteger (f+1)) +
	         fromInt e * log (fromInteger b)) /
		  fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt base n * s then n else fixup (n+1)
      else
        if expt base (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * base) `divMod` sN
    mUpN' = mUpN * base
    mDnN' = mDnN * base
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'
  
  rds = 
   if k >= 0 then
      gen [] r (s * expt base k) mUp mDn
   else
     let bk = expt base (-k) in
     gen [] (r * bk) s (mUp * bk) (mDn * bk)
 in
 (map toInt (reverse rds), k)

\end{code}

@showRational@ converts a Rational to a string that looks like a
floating point number, but without converting to any floating type
(because of the possible overflow).

From/by Lennart, 94/09/26

\begin{code}
showRational :: Int -> Rational -> String
showRational n r =
    if r == 0 then
    	"0.0"
    else
	let (r', e) = normalize r
	in  prR n r' e

startExpExp = 4 :: Int

-- make sure 1 <= r < 10
normalize :: Rational -> (Rational, Int)
normalize r = if r < 1 then
		  case norm startExpExp (1 / r) 0 of (r', e) -> (10 / r', -e-1)
	      else
		  norm startExpExp r 0
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
prR n r e | r <  1  = prR n (r*10) (e-1)		-- final adjustment
prR n r e | r >= 10 = prR n (r/10) (e+1)
prR n r e0 =
	let s = show ((round (r * 10^n))::Integer)
	    e = e0+1
	in  if e > 0 && e < 8 then
		take e s ++ "." ++ drop0 (drop e s)
	    else if e <= 0 && e > -3 then
	        "0." ++ take (-e) (repeat '0') ++ drop0 s
	    else
	        head s : "."++ drop0 (tail s) ++ "e" ++ show e0
\end{code}


[In response to a request for documentation of how fromRational works,
Joe Fasel writes:] A quite reasonable request!  This code was added to
the Prelude just before the 1.2 release, when Lennart, working with an
early version of hbi, noticed that (read . show) was not the identity
for floating-point numbers.  (There was a one-bit error about half the
time.)  The original version of the conversion function was in fact
simply a floating-point divide, as you suggest above. The new version
is, I grant you, somewhat denser.

Unfortunately, Joe's code doesn't work!  Here's an example:

main = putStr (shows (1.82173691287639817263897126389712638972163e-300::Double) "\n")

This program prints
	0.0000000000000000
instead of
	1.8217369128763981e-300

Lennart's code follows, and it works...

\begin{pseudocode}
{-# GENERATE_SPECS fromRational__ a{Double#,Double} #-}
fromRat :: (RealFloat a) => Rational -> a
fromRat x = x'
	where x' = f e

--		If the exponent of the nearest floating-point number to x 
--		is e, then the significand is the integer nearest xb^(-e),
--		where b is the floating-point radix.  We start with a good
--		guess for e, and if it is correct, the exponent of the
--		floating-point number we construct will again be e.  If
--		not, one more iteration is needed.

	      f e   = if e' == e then y else f e'
		      where y	   = encodeFloat (round (x * (1 % b)^^e)) e
			    (_,e') = decodeFloat y
	      b	    = floatRadix x'

--		We obtain a trial exponent by doing a floating-point
--		division of x's numerator by its denominator.  The
--		result of this division may not itself be the ultimate
--		result, because of an accumulation of three rounding
--		errors.

	      (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
					/ fromInteger (denominator x))
\end{pseudocode}

Now, here's Lennart's code.

\begin{code}
--fromRat :: (RealFloat a) => Rational -> a
fromRat x = 
    if x == 0 then encodeFloat 0 0 		-- Handle exceptional cases
    else if x < 0 then - fromRat' (-x)		-- first.
    else fromRat' x

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.

fromRat' :: (RealFloat a) => Rational -> a
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
	(minExp0, _) = floatRange r
	minExp = minExp0 - p		-- the real minimum exponent
	xMin = toRational (expt b (p-1))
	xMax = toRational (expt b p)
	p0 = (integerLogBase b (numerator x) - integerLogBase b (denominator x) - p) `max` minExp
	f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
	(x', p') = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
	r = encodeFloat (round x') p'

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x =
    if p <= minExp then
        (x, p)
    else if x >= xMax then
        scaleRat b minExp xMin xMax (p+1) (x/b)
    else if x < xMin  then
        scaleRat b minExp xMin xMax (p-1) (x*b)
    else
        (x, p)

-- Exponentiation with a cache for the most common numbers.
minExpt = 0::Int
maxExpt = 1100::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        base^n
expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

-- Compute the (floor of the) log of i in base b.
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
\end{code}


%*********************************************************
%*							*
\subsection{Numeric primops}
%*							*
%*********************************************************

Definitions of the boxed PrimOps; these will be
used in the case of partial applications, etc.

\begin{code}
plusFloat   (F# x) (F# y) = F# (plusFloat# x y)
minusFloat  (F# x) (F# y) = F# (minusFloat# x y)
timesFloat  (F# x) (F# y) = F# (timesFloat# x y)
divideFloat (F# x) (F# y) = F# (divideFloat# x y)
negateFloat (F# x)        = F# (negateFloat# x)

gtFloat	    (F# x) (F# y) = gtFloat# x y
geFloat	    (F# x) (F# y) = geFloat# x y
eqFloat	    (F# x) (F# y) = eqFloat# x y
neFloat	    (F# x) (F# y) = neFloat# x y
ltFloat	    (F# x) (F# y) = ltFloat# x y
leFloat	    (F# x) (F# y) = leFloat# x y

float2Int   (F# x) = I# (float2Int# x)
int2Float   (I# x) = F# (int2Float# x)

expFloat    (F# x) = F# (expFloat# x)
logFloat    (F# x) = F# (logFloat# x)
sqrtFloat   (F# x) = F# (sqrtFloat# x)
sinFloat    (F# x) = F# (sinFloat# x)
cosFloat    (F# x) = F# (cosFloat# x)
tanFloat    (F# x) = F# (tanFloat# x)
asinFloat   (F# x) = F# (asinFloat# x)
acosFloat   (F# x) = F# (acosFloat# x)
atanFloat   (F# x) = F# (atanFloat# x)
sinhFloat   (F# x) = F# (sinhFloat# x)
coshFloat   (F# x) = F# (coshFloat# x)
tanhFloat   (F# x) = F# (tanhFloat# x)

powerFloat  (F# x) (F# y) = F# (powerFloat# x y)

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusDouble   (D# x) (D# y) = D# (x +## y)
minusDouble  (D# x) (D# y) = D# (x -## y)
timesDouble  (D# x) (D# y) = D# (x *## y)
divideDouble (D# x) (D# y) = D# (x /## y)
negateDouble (D# x)        = D# (negateDouble# x)

gtDouble    (D# x) (D# y) = x >## y
geDouble    (D# x) (D# y) = x >=## y
eqDouble    (D# x) (D# y) = x ==## y
neDouble    (D# x) (D# y) = x /=## y
ltDouble    (D# x) (D# y) = x <## y
leDouble    (D# x) (D# y) = x <=## y

double2Int   (D# x) = I# (double2Int#   x)
int2Double   (I# x) = D# (int2Double#   x)
double2Float (D# x) = F# (double2Float# x)
float2Double (F# x) = D# (float2Double# x)

expDouble    (D# x) = D# (expDouble# x)
logDouble    (D# x) = D# (logDouble# x)
sqrtDouble   (D# x) = D# (sqrtDouble# x)
sinDouble    (D# x) = D# (sinDouble# x)
cosDouble    (D# x) = D# (cosDouble# x)
tanDouble    (D# x) = D# (tanDouble# x)
asinDouble   (D# x) = D# (asinDouble# x)
acosDouble   (D# x) = D# (acosDouble# x)
atanDouble   (D# x) = D# (atanDouble# x)
sinhDouble   (D# x) = D# (sinhDouble# x)
coshDouble   (D# x) = D# (coshDouble# x)
tanhDouble   (D# x) = D# (tanhDouble# x)

powerDouble  (D# x) (D# y) = D# (x **## y)
\end{code}
