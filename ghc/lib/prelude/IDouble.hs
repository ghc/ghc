module PreludeCore ( Double(..) ) where

#include "../includes/ieee-flpt.h"

import Cls
import Core
import IInt
import IInteger
import IRatio
import List		( (++) )
import Prel		( (^), (^^), otherwise )
import PS		( _PackedString, _unpackPS )
import Text
import TyComplex    -- for pragmas only

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
    abs x | x >= 0	=  x
	  | otherwise	=  negateDouble x
    signum x | x == 0	 =  0
	     | x > 0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  case (int2Double# n#) of { d# -> D# d# }

instance  Real Double  where
    toRational x	=  (m%1)*(b%1)^^n -- i.e., realFloatToRational x
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double  where
    (/) x y		=  divideDouble x y
    fromRational x	=  fromRationalX x --ORIG: rationalToRealFloat x
    recip x		=  1 / x

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

{- WAS: but not all machines have these in their math library:
    asinh		=  asinhDouble
    acosh		=  acoshDouble
    atanh		=  atanhDouble
-}
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))


instance  RealFrac Double  where
    properFraction x = _properFraction x

    -- just call the versions in Core.hs
    truncate x	=  _truncate x
    round x	=  _round x
    ceiling x	=  _ceiling x
    floor x	=  _floor x
{- OLD:
    properFraction x
	| n >= 0	=  (fromInteger m * fromInteger b ^ n, 0)
	| otherwise	=  (fromInteger w, encodeFloat r n)
			where (m,n) = decodeFloat x
			      b	    = floatRadix x
			      (w,r) = quotRem m (b^(-n))
-}

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

instance  Enum Double  where
{- *** RAW PRELUDE ***
    enumFrom		=  numericEnumFrom
    enumFromThen	=  numericEnumFromThen
-}
    enumFrom x = x : enumFrom (x `plusDouble` 1.0)
    enumFromThen m n = en' m (n `minusDouble` m)
	    where en' m n = m : en' (m `plusDouble` n) n

instance  Text Double  where
    readsPrec p x = readSigned readFloat x
    showsPrec   x = showSigned showFloat x

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
    abs x | x >= 0	=  x
	  | otherwise	=  negateDouble# x
    signum x | x == 0	 =  0
	     | x > 0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  int2Double# n#

instance  Real Double#  where
    toRational x	=  (m%1)*(b%1)^^n -- i.e., realFloatToRational x
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double#  where
    (/) x y		=  divideDouble# x y
    fromRational x	=  _fromRational x --ORIG: rationalToRealFloat x
    recip x		=  1 / x

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

{- WAS: but not all machines have these in their math library:
    asinh		=  asinhDouble#
    acosh		=  acoshDouble#
    atanh		=  atanhDouble#
-}
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))


instance  RealFrac Double#  where
    -- REPORT:
    -- properFraction = floatProperFraction

    properFraction x
	| n >= 0	=  (fromInteger m * fromInteger b ^ n, 0)
	| otherwise	=  (fromInteger w, encodeFloat r n)
			where (m,n) = decodeFloat x
			      b	    = floatRadix x
			      (w,r) = quotRem m (b^(-n))

    -- No default methods for unboxed values ...
    -- just call the versions in Core.hs
    truncate x	=  _truncate x
    round x	=  _round x
    ceiling x	=  _ceiling x
    floor x	=  _floor x

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

    -- No default methods for unboxed values ...
    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (- (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x

instance  Enum Double#  where
    enumFrom x           =  x : enumFrom (x `plusDouble#` 1.0##)
    enumFromThen m n     =  en' m (n `minusDouble#` m)
	                    where en' m n = m : en' (m `plusDouble#` n) n
    -- default methods not specialised!
    enumFromTo n m	 =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

-- ToDo: efficient Text Double# instance
instance  Text Double#  where
    readsPrec p s = map (\ (D# d#, s) -> (d#, s)) (readsPrec p s)
    showsPrec p x = showsPrec p (D# x)
    readList s = map (\ (x, s) -> (map (\ (D# d#) -> d#) x, s)) (readList s)
    showList l = showList (map D# l)

instance _CCallable   Double#
instance _CReturnable Double#

#endif {-UNBOXED INSTANCES-}
