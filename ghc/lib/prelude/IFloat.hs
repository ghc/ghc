module PreludeCore ( Float(..) ) where

#include "../includes/ieee-flpt.h"

import Cls
import Core
import IInt
import IInteger
import IRatio
import List		( (++), takeWhile )
import Prel		( (^), (^^), otherwise )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

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

---------------------------------------------------------------

instance  Eq Float  where
    (==) x y = eqFloat x y
    (/=) x y = neFloat x y

instance  Ord Float  where
    (<=) x y = leFloat x y
    (<)	 x y = ltFloat x y
    (>=) x y = geFloat x y
    (>)	 x y = gtFloat x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (F# a#) (F# b#)
      = if      (eqFloat# a# b#) then _EQ
	else if (ltFloat# a# b#) then _LT else _GT

instance  Num Float  where
    (+)		x y 	=  plusFloat x y
    (-)		x y 	=  minusFloat x y
    negate	x  	=  negateFloat x
    (*)		x y 	=  timesFloat x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateFloat x
    signum x | x == 0.0	 =  0
	     | x > 0.0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt i		=  int2Float i

instance  Real Float  where
    toRational x	=  (m%__i1)*(b%__i1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Float  where
    (/) x y		=  divideFloat x y
    fromRational x	=  _fromRational x
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
	    case (quotRem m (b^(-n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < 0.0 then n - __i1 else n + __i1
		  	      	half_down = abs r - 0.5
    		   	      in
    		   	      case (_tagCmp half_down 0.0) of
      		     		_LT -> n
      		     		_EQ -> if even n then n else m
      		     		_GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > 0.0 then n + __i1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < 0.0 then n - __i1 else n

instance  RealFloat Float  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  FLT_MANT_DIG	    -- ditto
    floatRange _	=  (FLT_MIN_EXP, FLT_MAX_EXP) -- ditto

    decodeFloat (F# f#)
      = case decodeFloat# f#	of
	  _ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeFloat# a# s# d# e# of { flt# -> F# flt# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == __i0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

instance  Enum Float  where
    enumFrom x           =  x : enumFrom (x `plusFloat` 1.0)
    enumFromThen m n     =  en' m (n `minusFloat` m)
	                    where en' m n = m : en' (m `plusFloat` n) n
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Text Float  where
    readsPrec p x = readSigned readFloat x
    showsPrec   x = showSigned showFloat x
    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0) 

---------------------------------------------------------------
instance _CCallable   Float
instance _CReturnable Float
