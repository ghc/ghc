module PreludeCore ( Double(..) ) where

#include "../includes/ieee-flpt.h"

import Cls
import Core
import IInt
import IInteger
import IRatio
import List		( (++), map, takeWhile )
import Prel		( (^), (^^), otherwise )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusDouble   (D# x) (D# y) = D# (plusDouble# x y)
minusDouble  (D# x) (D# y) = D# (minusDouble# x y)
timesDouble  (D# x) (D# y) = D# (timesDouble# x y)
divideDouble (D# x) (D# y) = D# (divideDouble# x y)
negateDouble (D# x)        = D# (negateDouble# x)

gtDouble    (D# x) (D# y) = gtDouble# x y
geDouble    (D# x) (D# y) = geDouble# x y
eqDouble    (D# x) (D# y) = eqDouble# x y
neDouble    (D# x) (D# y) = neDouble# x y
ltDouble    (D# x) (D# y) = ltDouble# x y
leDouble    (D# x) (D# y) = leDouble# x y

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

powerDouble  (D# x) (D# y) = D# (powerDouble# x y)

---------------------------------------------------------------

instance  Eq Double  where
    (==) x y = eqDouble x y
    (/=) x y = neDouble x y

instance  Ord Double  where
    (<=) x y = leDouble x y
    (<)	 x y = ltDouble x y
    (>=) x y = geDouble x y
    (>)	 x y = gtDouble x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (D# a#) (D# b#)
      = if      (eqDouble# a# b#) then _EQ
	else if (ltDouble# a# b#) then _LT else _GT

instance  Num Double  where
    (+)		x y 	=  plusDouble x y
    (-)		x y 	=  minusDouble x y
    negate	x  	=  negateDouble x
    (*)		x y 	=  timesDouble x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateDouble x
    signum x | x == 0.0	 =  0
	     | x > 0.0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  case (int2Double# n#) of { d# -> D# d# }

instance  Real Double  where
    toRational x	=  (m%__i1)*(b%__i1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double  where
    (/) x y		=  divideDouble x y
    fromRational x	=  _fromRational x
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

instance  RealFloat Double  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  DBL_MANT_DIG	    -- ditto
    floatRange _	=  (DBL_MIN_EXP, DBL_MAX_EXP) -- ditto

    decodeFloat (D# d#)
      = case decodeDouble# d#	of
	  _ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeDouble# a# s# d# e#	of { dbl# -> D# dbl# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == __i0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

instance  Enum Double  where
    enumFrom x           =  x : enumFrom (x `plusDouble` 1.0)
    enumFromThen m n     =  en' m (n `minusDouble` m)
	                    where en' m n = m : en' (m `plusDouble` n) n
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				     (enumFromThen n m)

instance  Text Double  where
    readsPrec p x = readSigned readFloat x
    showsPrec   x = showSigned showFloat x
    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0) 

instance _CCallable   Double
instance _CReturnable Double

#if defined(__UNBOXED_INSTANCES__)
---------------------------------------------------------------
-- Instances for Double#
---------------------------------------------------------------

instance  Eq Double#  where
    (==) x y = eqDouble# x y
    (/=) x y = neDouble# x y

instance  Ord Double#  where
    (<=) x y = leDouble# x y
    (<)	 x y = ltDouble# x y
    (>=) x y = geDouble# x y
    (>)	 x y = gtDouble# x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp a b
      = if      (eqDouble# a b) then _EQ
	else if (ltDouble# a b) then _LT else _GT

instance  Num Double#  where
    (+)		x y 	=  plusDouble# x y
    (-)		x y 	=  minusDouble# x y
    negate	x  	=  negateDouble# x
    (*)		x y 	=  timesDouble# x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateDouble# x
    signum x | x == 0.0	 =  0
	     | x > 0.0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  int2Double# n#

instance  Real Double#  where
    toRational x	=  (m%__i1)*(b%__i1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double#  where
    (/) x y		=  divideDouble# x y
    fromRational x	=  _fromRational x
    recip x		=  1.0 / x

instance  Floating Double#  where
    pi			=  3.141592653589793238##
    exp	x		=  expDouble# x
    log	x		=  logDouble# x
    sqrt x		=  sqrtDouble# x
    sin	 x		=  sinDouble# x
    cos	 x		=  cosDouble# x
    tan	 x		=  tanDouble# x
    asin x		=  asinDouble# x
    acos x	 	=  acosDouble# x
    atan x		=  atanDouble# x
    sinh x		=  sinhDouble# x
    cosh x		=  coshDouble# x
    tanh x		=  tanhDouble# x
    (**) x y		=  powerDouble# x y
    logBase x y		=  log y / log x

    asinh x = log (x + sqrt (1.0+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1.0)/(x+1.0)))
    atanh x = log ((x+1.0) / sqrt (1.0-x*x))


instance  RealFrac Double#  where

    {-# SPECIALIZE properFraction :: Double# -> (Int, Double#) #-}
    {-# SPECIALIZE truncate :: Double# -> Int #-}
    {-# SPECIALIZE round    :: Double# -> Int #-}
    {-# SPECIALIZE ceiling  :: Double# -> Int #-}
    {-# SPECIALIZE floor    :: Double# -> Int #-}

    {-# SPECIALIZE properFraction :: Double# -> (Integer, Double#) #-}
    {-# SPECIALIZE truncate :: Double# -> Integer #-}
    {-# SPECIALIZE round    :: Double# -> Integer #-}
    {-# SPECIALIZE ceiling  :: Double# -> Integer #-}
    {-# SPECIALIZE floor    :: Double# -> Integer #-}

    {-# SPECIALIZE properFraction :: Double# -> (Int#, Double#) #-}
    {-# SPECIALIZE truncate :: Double# -> Int# #-}
    {-# SPECIALIZE round    :: Double# -> Int# #-}
    {-# SPECIALIZE ceiling  :: Double# -> Int# #-}
    {-# SPECIALIZE floor    :: Double# -> Int# #-}

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


instance  RealFloat Double#  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  DBL_MANT_DIG	    -- ditto
    floatRange _	=  (DBL_MIN_EXP, DBL_MAX_EXP) -- ditto

    decodeFloat d#
      = case decodeDouble# d#	of
	  _ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = encodeDouble# a# s# d# e#

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == __i0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

instance  Enum Double#  where
    enumFrom x           =  x : enumFrom (x `plusDouble#` 1.0)
    enumFromThen m n     =  en' m (n `minusDouble#` m)
	                    where en' m n = m : en' (m `plusDouble#` n) n
    enumFromTo n m	 =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

-- ToDo: efficient Text Double# instance
instance  Text Double#  where
    readsPrec p s = map (\ (D# d#, s) -> (d#, s)) (readsPrec p s)
    showsPrec p x = showsPrec p (D# x)
    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0)

instance _CCallable   Double#
instance _CReturnable Double#

#endif {-UNBOXED INSTANCES-}
