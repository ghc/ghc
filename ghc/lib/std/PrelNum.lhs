%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelNum]{Module @PrelNum@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelNum where

import PrelBase
import Ix
import {-# SOURCE #-} PrelErr

infixr 8  ^, ^^, **
infixl 7  %, /, `quot`, `rem`, `div`, `mod`
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

    n `quot` d		=  q  where (q,_) = quotRem n d
    n `rem` d		=  r  where (_,r) = quotRem n d
    n `div` d		=  q  where (q,_) = divMod n d
    n `mod` d		=  r  where (_,r) = divMod n d
    divMod n d 		=  if signum r == negate (signum d) then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a

    recip x		=  1 / x
    x / y		= x * recip y

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
    atan2	        :: a -> a -> a


    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (negate (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x
			   
    atan2 y x
      | x > 0            =  atan (y/x)
      | x == 0 && y > 0  =  pi/2
      | x <  0 && y > 0  =  pi + atan (y/x) 
      |(x <= 0 && y < 0)            ||
       (x <  0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                         = -atan2 (-y) x
      | y == 0 && (x < 0 || isNegativeZero x)
                          =  pi    -- must be after the previous test on zero y
      | x==0 && y==0      =  y     -- must be after the other double zero tests
      | otherwise         =  x + y -- x or y is a NaN, return a NaN (via +)

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

    -- Following chks for zero divisor are non-standard (WDP)
    a `quot` b	=  if b /= 0
		   then a `quotInt` b
		   else error "Prelude.Integral{Int}.quot: divide by 0\n"
    a `rem` b	=  if b /= 0
		   then a `remInt` b
		   else error "Prelude.Integral{Int}.rem: divide by 0\n"

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

    toInteger (I# i)  = int2Integer i  -- give back a full-blown Integer
    toInt x	      = x

\end{code}

%*********************************************************
%*							*
\subsection{Instances for @Integer@}
%*							*
%*********************************************************

\begin{code}
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
      = case plusInteger# a1 s1 d1 a2 s2 d2 of (# a, s, d #) -> J# a s d

    (-) (J# a1 s1 d1) (J# a2 s2 d2)
      = case minusInteger# a1 s1 d1 a2 s2 d2 of (# a, s, d #) -> J# a s d

    negate (J# a s d) 
      = case negateInteger# a s d of (# a1, s1, d1 #) -> J# a1 s1 d1

    (*) (J# a1 s1 d1) (J# a2 s2 d2)
      = case timesInteger# a1 s1 d1 a2 s2 d2 of (# a, s, d #) -> J# a s d

    -- ORIG: abs n = if n >= 0 then n else -n

    abs n@(J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	if (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#
	then n
	else case negateInteger# a1 s1 d1 of (# a, s, d #) -> J# a s d
	}

    signum (J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	let
	    cmp = cmpInteger# a1 s1 d1 a2 s2 d2
	in
	if      cmp >#  0# then 1
	else if cmp ==# 0# then 0
	else			(negate 1)
	}

    fromInteger	x	=  x

    fromInt (I# i)	=  int2Integer i

instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
    quotRem (J# a1 s1 d1) (J# a2 s2 d2)
      = case (quotRemInteger# a1 s1 d1 a2 s2 d2) of
	  (# a3, s3, d3, a4, s4, d4 #)
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
    n `quot` d	=  if d /= 0 then q else 
		     error "Prelude.Integral{Integer}.quot: divide by 0\n"  
		   where (q,_) = quotRem n d
    n `rem` d	=  if d /= 0 then r else 
		     error "Prelude.Integral{Integer}.rem: divide by 0\n"  
		   where (_,r) = quotRem n d
    n `div` d	=  q  where (q,_) = divMod n d
    n `mod` d	=  r  where (_,r) = divMod n d

    divMod n d 	=  case (quotRem n d) of { qr@(q,r) ->
		   if signum r == negate (signum d) then (q - 1, r+d) else qr }
		   -- Case-ified by WDP 94/10

instance  Enum Integer  where
    succ x		 = x + 1
    pred x		 = x - 1
    toEnum n		 =  toInteger n
    fromEnum n		 =  toInt n
    enumFrom n           =  n : enumFrom (n + 1)
    enumFromThen m n     =  en' m (n - m)
	                    where en' a b = a : en' (a + b) b
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Show Integer  where
    showsPrec   x = showSignedInteger x
    showList = showList__ (showsPrec 0) 


instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index b@(m,_) i
	| inRange b i	=  fromInteger (i - m)
	| otherwise	=  indexIntegerError i b
    inRange (m,n) i	=  m <= i && i <= n

-- Sigh, really want to use helper function in Ix, but
-- module deps. are too painful.
{-# NOINLINE indexIntegerError #-}
indexIntegerError :: Integer -> (Integer,Integer) -> a
indexIntegerError i rng
  = error (showString "Ix{Integer}.index: Index " .
           showParen True (showsPrec 0 i) .
	   showString " out of range " $
	   showParen True (showsPrec 0 rng) "")

showSignedInteger :: Int -> Integer -> ShowS
showSignedInteger p n r
  | n < 0 && p > 6 = '(':jtos n (')':r)
  | otherwise      = jtos n r

jtos :: Integer -> String -> String
jtos i rs
 | i < 0     = '-' : jtos' (-i) rs
 | otherwise = jtos' i rs
 where
  jtos' :: Integer -> String -> String
  jtos' n cs
   | n < 10    = chr (fromInteger n + (ord_0::Int)) : cs
   | otherwise = jtos' q (chr (toInt r + (ord_0::Int)) : cs)
    where
     (q,r) = n `quotRem` 10
\end{code}

%*********************************************************
%*							*
\subsection{The @Ratio@ and @Rational@ types}
%*							*
%*********************************************************

\begin{code}
data  (Integral a)	=> Ratio a = !a :% !a  deriving (Eq)
type  Rational		=  Ratio Integer

{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a
\end{code}

\tr{reduce} is a subsidiary function used only in this module .
It normalises a ratio by dividing both numerator and denominator by
their greatest common divisor.

\begin{code}
reduce ::  (Integral a) => a -> a -> Ratio a
reduce _ 0		=  error "Ratio.%: zero denominator"
reduce x y		=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y
\end{code}

\begin{code}
x % y			=  reduce (x * signum y) (abs y)

numerator   (x :% _)	=  x
denominator (_ :% y)	=  y

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

{-# SPECIALISE gcd ::
	Int -> Int -> Int,
	Integer -> Integer -> Integer #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' a 0  =  a
			 gcd' a b  =  gcd' b (a `rem` b)

{-# SPECIALISE lcm ::
	Int -> Int -> Int,
	Integer -> Integer -> Integer #-}
lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

{-# SPECIALISE (^) ::
	Integer -> Integer -> Integer,
	Integer -> Int -> Integer,
	Int -> Int -> Int #-}
(^)		:: (Num a, Integral b) => a -> b -> a
_ ^ 0		=  1
x ^ n | n > 0	=  f x (n-1) x
		   where f _ 0 y = y
		         f a d y = g a d  where
			           g b i | even i  = g (b*b) (i `quot` 2)
				         | otherwise = f b (i-1) (b*y)
_ ^ _		= error "Prelude.^: negative exponent"

{- SPECIALISE (^^) ::
	Double -> Int -> Double,
	Rational -> Int -> Rational #-}
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(negate n))

\end{code}

