%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelReal]{Module @PrelReal@}

The types

	Ratio, Rational

and the classes

	Real
	Integral
	Fractional
	RealFrac


\begin{code}
{-# OPTIONS -fcompiling-prelude -fno-implicit-prelude #-}

module PrelReal where

import {-# SOURCE #-} PrelErr
import PrelBase
import PrelNum
import PrelList
import PrelEnum
import PrelShow

infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`

default ()		-- Double isn't available yet, 
			-- and we shouldn't be using defaults anyway
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
\end{code}


These 'numeric' enumerations come straight from the Report

\begin{code}
numericEnumFrom		:: (Fractional a) => a -> [a]
numericEnumFrom		=  iterate (+1)

numericEnumFromThen	:: (Fractional a) => a -> a -> [a]
numericEnumFromThen n m	=  iterate (+(m-n)) n

numericEnumFromTo       :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromTo n m   = takeWhile (<= m + 1/2) (numericEnumFrom n)

numericEnumFromThenTo   :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo e1 e2 e3 = takeWhile pred (numericEnumFromThen e1 e2)
				where
				 mid = (e2 - e1) / 2
				 pred | e2 > e1   = (<= e3 + mid)
				      | otherwise = (>= e3 + mid)
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
    toInteger i = int2Integer i  -- give back a full-blown Integer
    toInt x	= x

    -- Following chks for zero divisor are non-standard (WDP)
    a `quot` b	=  if b /= 0
		   then a `quotInt` b
		   else error "Prelude.Integral.quot{Int}: divide by 0"
    a `rem` b	=  if b /= 0
		   then a `remInt` b
		   else error "Prelude.Integral.rem{Int}: divide by 0"

    x `div` y = x `divInt` y
    x `mod` y = x `modInt` y

    a `quotRem` b = a `quotRemInt` b
    a `divMod`  b = a `divModInt`  b
\end{code}


%*********************************************************
%*							*
\subsection{Instances for @Integer@}
%*							*
%*********************************************************

\begin{code}
instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
    toInteger n	     = n
    toInt n	     = integer2Int n

    n `quot` d = n `quotInteger` d
    n `rem`  d = n `remInteger`  d

    n `div` d	=  q  where (q,_) = divMod n d
    n `mod` d	=  r  where (_,r) = divMod n d

    a `divMod` b = a `divModInteger` b
    a `quotRem` b = a `quotRemInteger` b
\end{code}


%*********************************************************
%*							*
\subsection{Instances for @Ratio@}
%*							*
%*********************************************************

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
    signum (x:%_)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
			  where (q,r) = quotRem x y

instance  (Integral a)  => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > ratio_prec)
    	    	    	       (shows x . showString " % " . shows y)

ratio_prec :: Int
ratio_prec = 7

instance  (Integral a)	=> Enum (Ratio a)  where
    succ x	        =  x + 1
    pred x	        =  x - 1

    toEnum n            =  fromInt n :% 1
    fromEnum            =  fromInteger . truncate

    enumFrom		=  numericEnumFrom
    enumFromThen 	=  numericEnumFromThen
    enumFromTo		=  numericEnumFromTo
    enumFromThenTo	=  numericEnumFromThenTo
\end{code}


%*********************************************************
%*							*
\subsection{Overloaded numeric functions}
%*							*
%*********************************************************

\begin{code}
showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x 
   | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x

even, odd	:: (Integral a) => a -> Bool
even n		=  n `rem` 2 == 0
odd		=  not . even

-------------------------------------------------------
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
	Rational -> Int -> Rational #-}
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(negate n))


-------------------------------------------------------
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' a 0  =  a
			 gcd' a b  =  gcd' b (a `rem` b)

lcm		:: (Integral a) => a -> a -> a
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)


{-# RULES
"Int.gcd"      forall a b . gcd  a b = gcdInt a b
"Integer.gcd"  forall a b . gcd  a b = gcdInteger  a b
"Integer.lcm"  forall a b . lcm  a b = lcmInteger  a b
 #-}
\end{code}
