\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Real
-- Copyright   :  (c) The FFI Task Force, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Ratio' and 'Rational', and the classes 'Real', 'Fractional',
-- 'Integral', and 'RealFrac'.
--
-----------------------------------------------------------------------------

module GHC.Real where

import {-# SOURCE #-} GHC.Err
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum
import GHC.Show

infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`
infixl 7  %

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

-- | Arbitrary-precision rational numbers, represented as a ratio of
-- two 'Integer' values.  A rational number may be constructed using
-- the '%' operator.
type  Rational		=  Ratio Integer

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7 	-- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

infinity, notANumber :: Rational
infinity   = 1 :% 0
notANumber = 0 :% 0

-- Use :%, not % for Inf/NaN; the latter would 
-- immediately lead to a runtime error, because it normalises. 
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
{-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
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
				 pred | e2 >= e1  = (<= e3 + mid)
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

    a `quot` 0   = divZeroError
    a `quot` b	=  a `quotInt` b

    a `rem` 0   = divZeroError
    a `rem` b	= a `remInt` b

    a `div` 0   = divZeroError
    a `div` b   = a `divInt` b

    a `mod` 0   = divZeroError
    a `mod` b   = a `modInt` b

    a `quotRem` 0 = divZeroError
    a `quotRem` b = a `quotRemInt` b

    a `divMod`  0 = divZeroError
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

    a `quot` 0 = divZeroError
    n `quot` d = n `quotInteger` d

    a `rem` 0 = divZeroError
    n `rem`  d = n `remInteger`  d

    a `divMod` 0 = divZeroError
    a `divMod` b = a `divModInteger` b

    a `quotRem` 0 = divZeroError
    a `quotRem` b = a `quotRemInteger` b

    -- use the defaults for div & mod
\end{code}


%*********************************************************
%*							*
\subsection{Instances for @Ratio@}
%*							*
%*********************************************************

\begin{code}
instance  (Integral a)	=> Ord (Ratio a)  where
    {-# SPECIALIZE instance Ord Rational #-}
    (x:%y) <= (x':%y')	=  x * y' <= x' * y
    (x:%y) <  (x':%y')	=  x * y' <  x' * y

instance  (Integral a)	=> Num (Ratio a)  where
    {-# SPECIALIZE instance Num Rational #-}
    (x:%y) + (x':%y')	=  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')	=  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%_)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Fractional (Ratio a)  where
    {-# SPECIALIZE instance Fractional Rational #-}
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  y % x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> Real (Ratio a)  where
    {-# SPECIALIZE instance Real Rational #-}
    toRational (x:%y)	=  toInteger x :% toInteger y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    {-# SPECIALIZE instance RealFrac Rational #-}
    properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
			  where (q,r) = quotRem x y

instance  (Integral a)  => Show (Ratio a)  where
    {-# SPECIALIZE instance Show Rational #-}
    showsPrec p (x:%y)	=  showParen (p > ratioPrec) $
			   showsPrec ratioPrec1 x . 
			   showString " % " . 
			   showsPrec ratioPrec1 y

instance  (Integral a)	=> Enum (Ratio a)  where
    {-# SPECIALIZE instance Enum Rational #-}
    succ x	        =  x + 1
    pred x	        =  x - 1

    toEnum n            =  fromInteger (int2Integer n) :% 1
    fromEnum            =  fromInteger . truncate

    enumFrom		=  numericEnumFrom
    enumFromThen 	=  numericEnumFromThen
    enumFromTo		=  numericEnumFromTo
    enumFromThenTo	=  numericEnumFromThenTo
\end{code}


%*********************************************************
%*							*
\subsection{Coercions}
%*							*
%*********************************************************

\begin{code}
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

{-# RULES
"fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

{-# RULES
"realToFrac/Int->Int" realToFrac = id :: Int -> Int
    #-}
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

{-# SPECIALISE (^^) ::
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
"gcd/Int->Int->Int"             gcd = gcdInt
"gcd/Integer->Integer->Integer" gcd = gcdInteger
"lcm/Integer->Integer->Integer" lcm = lcmInteger
 #-}

integralEnumFrom :: (Integral a, Bounded a) => a -> [a]
integralEnumFrom n = map fromInteger [toInteger n .. toInteger (maxBound `asTypeOf` n)]

integralEnumFromThen :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThen n1 n2
  | i_n2 >= i_n1  = map fromInteger [i_n1, i_n2 .. toInteger (maxBound `asTypeOf` n1)]
  | otherwise     = map fromInteger [i_n1, i_n2 .. toInteger (minBound `asTypeOf` n1)]
  where
    i_n1 = toInteger n1
    i_n2 = toInteger n2

integralEnumFromTo :: Integral a => a -> a -> [a]
integralEnumFromTo n m = map fromInteger [toInteger n .. toInteger m]

integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]
integralEnumFromThenTo n1 n2 m
  = map fromInteger [toInteger n1, toInteger n2 .. toInteger m]
\end{code}
