module PreludeCore (
	Eq(..), Ord(..), Num(..), Real(..), Integral(..),
	Fractional(..), Floating(..), RealFrac(..), RealFloat(..),
	Ix(..), Enum(..), Text(..), Binary(..),
	_CCallable(..), _CReturnable(..),
	Bin
    ) where

import UTypes

import Core
import IChar
import IInt		( _rangeComplaint_Ix_Int, Int )
import IInteger 	( __integer1, Integer )
import List		( takeWhile, (++), foldr )
import Prel		( (&&), (.), otherwise )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

{- We have to do something unpleasant about overloaded constants
   herein.  Those constants are automagically wrapped in applications
   of the *BUILT-IN* from{Integer,Rational} Ids.
   
   Those are *NOT* the same methods as those being compiled here!
   (The builtin class information is "turned off" for compiling this
   file, but that does not help w/ the from{Integer,Rational} Ids,
   which are reached-out-and-grabbed from thin air.

   Instead the overloaded constants are declared in Core.hs
-}

-- class declarations from PreludeCore

class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

    x /= y = if x == y then False else True

class  (Eq a) => Ord a  where
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> a
    -- NON-STANDARD GLASGOW ADDITION:
    _tagCmp :: a -> a -> _CMP_TAG

    x <  y =  x <= y && x /= y
    x >= y =  y <= x
    x >  y =  y <  x
    max x y | x >= y	=  x
            | y >= x	=  y
            |otherwise	=  error "max{PreludeCore}: no ordering relation\n"
    min x y | x <= y	=  x
            | y <= x	=  y
            |otherwise	=  error "min{PreludeCore}: no ordering relation\n"
    _tagCmp a b = if a == b then _EQ else if a < b then _LT else _GT

class  (Eq a, Text a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a		-- partain: extra! (see note below)

    x - y 		= x + negate y
    fromInt i		= fromInteger (int2Integer i)
			where
			  int2Integer (I# i#) = int2Integer# i#
					-- Go via the standard class-op if the
					-- non-standard one ain't provided

{-
Note: Both GHC and HBC provide an extra class operation in @Num@,
namely @fromInt@.  This makes small overloaded literal constants, such
as ``42'', much more efficient.  Instead of building the @Integer@ for
``42'' and then converting that expensively to the desired type, we
can then just make the @Int@ for ``42'' and convert that to the
desired type.
-}

class  (Num a, Enum a) => Real a  where
    toRational		::  a -> Rational

class  (Real a, Ix a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    even, odd		:: a -> Bool
    toInteger		:: a -> Integer
    toInt		:: a -> Int		-- partain: also extra (as above)

    n `quot` d	=  q  where (q,r) = quotRem n d
    n `rem` d	=  r  where (q,r) = quotRem n d
    n `div` d	=  q  where (q,r) = divMod n d
    n `mod` d	=  r  where (q,r) = divMod n d
    divMod n d 	=  if signum r == - signum d then (q - __i1, r+d) else qr
			   where qr@(q,r) = quotRem n d
    even n	=  n `rem` __i2 == __i0
    odd	 n	=  n `rem` __i2 /= __i0

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a

    recip x		=  __i1 / x

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
    sqrt x		=  x ** __rhalf
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b,a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < __i0 then n - __i1 else n + __i1
		  	      	half_down = abs r - __rhalf
    		   	      in
    		   	      case (_tagCmp half_down __i0) of
      		     		_LT -> n
      		     		_EQ -> if even n then n else m
      		     		_GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > __i0 then n + __i1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < __i0 then n - __i1 else n


class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)
    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a

    exponent x		=  if m == __i0 then __i0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (- (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x

class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

class  (Ord a) => Enum a	where
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,m..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,m..p]

    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

class  Text a  where
    readsPrec :: Int -> ReadS a
    showsPrec :: Int -> a -> ShowS
    readList  :: ReadS [a]
    showList  :: [a] -> ShowS

    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0)

-- Well, we've got to put it somewhere...

instance  Text (a -> b)  where
    readsPrec p s  =  error "readsPrec{PreludeCore}: Cannot read functions."
    showsPrec p f  =  showString "<<function>>"
    readList	   =  _readList (readsPrec 0)
    showList	   =  _showList (showsPrec 0) 

class  Binary a  where
    readBin		:: Bin -> (a,Bin)
    showBin		:: a -> Bin -> Bin

class _CCallable a
class _CReturnable a
