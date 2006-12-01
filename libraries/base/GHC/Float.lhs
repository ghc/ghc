\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Float
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Float' and 'Double', and the classes 'Floating' and 'RealFloat'.
--
-----------------------------------------------------------------------------

#include "ieee-flpt.h"

-- #hide
module GHC.Float( module GHC.Float, Float#, Double# )  where

import Data.Maybe

import GHC.Base
import GHC.List
import GHC.Enum
import GHC.Show
import GHC.Num
import GHC.Real
import GHC.Arr

infixr 8  **
\end{code}

%*********************************************************
%*							*
\subsection{Standard numeric classes}
%*							*
%*********************************************************

\begin{code}
-- | Trigonometric and hyperbolic functions and related functions.
--
-- Minimal complete definition:
--      'pi', 'exp', 'log', 'sin', 'cos', 'sinh', 'cosh',
--      'asin', 'acos', 'atan', 'asinh', 'acosh' and 'atanh'
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

-- | Efficient, machine-independent access to the components of a
-- floating-point number.
--
-- Minimal complete definition:
--	all except 'exponent', 'significand', 'scaleFloat' and 'atan2'
class  (RealFrac a, Floating a) => RealFloat a  where
    -- | a constant function, returning the radix of the representation
    -- (often @2@)
    floatRadix		:: a -> Integer
    -- | a constant function, returning the number of digits of
    -- 'floatRadix' in the significand
    floatDigits		:: a -> Int
    -- | a constant function, returning the lowest and highest values
    -- the exponent may assume
    floatRange		:: a -> (Int,Int)
    -- | The function 'decodeFloat' applied to a real floating-point
    -- number returns the significand expressed as an 'Integer' and an
    -- appropriately scaled exponent (an 'Int').  If @'decodeFloat' x@
    -- yields @(m,n)@, then @x@ is equal in value to @m*b^^n@, where @b@
    -- is the floating-point radix, and furthermore, either @m@ and @n@
    -- are both zero or else @b^(d-1) <= m < b^d@, where @d@ is the value
    -- of @'floatDigits' x@.  In particular, @'decodeFloat' 0 = (0,0)@.
    decodeFloat		:: a -> (Integer,Int)
    -- | 'encodeFloat' performs the inverse of 'decodeFloat'
    encodeFloat		:: Integer -> Int -> a
    -- | the second component of 'decodeFloat'.
    exponent		:: a -> Int
    -- | the first component of 'decodeFloat', scaled to lie in the open
    -- interval (@-1@,@1@)
    significand		:: a -> a
    -- | multiplies a floating-point number by an integer power of the radix
    scaleFloat		:: Int -> a -> a
    -- | 'True' if the argument is an IEEE \"not-a-number\" (NaN) value
    isNaN		:: a -> Bool
    -- | 'True' if the argument is an IEEE infinity or negative infinity
    isInfinite		:: a -> Bool
    -- | 'True' if the argument is too small to be represented in
    -- normalized format
    isDenormalized	:: a -> Bool
    -- | 'True' if the argument is an IEEE negative zero
    isNegativeZero	:: a -> Bool
    -- | 'True' if the argument is an IEEE floating point number
    isIEEE		:: a -> Bool
    -- | a version of arctangent taking two real floating-point arguments.
    -- For real floating @x@ and @y@, @'atan2' y x@ computes the angle
    -- (from the positive x-axis) of the vector from the origin to the
    -- point @(x,y)@.  @'atan2' y x@ returns a value in the range [@-pi@,
    -- @pi@].  It follows the Common Lisp semantics for the origin when
    -- signed zeroes are supported.  @'atan2' y 1@, with @y@ in a type
    -- that is 'RealFloat', should return the same value as @'atan' y@.
    -- A default definition of 'atan2' is provided, but implementors
    -- can provide a more accurate implementation.
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
\subsection{Type @Integer@, @Float@, @Double@}
%*							*
%*********************************************************

\begin{code}
-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data Float	= F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data Double	= D# Double#
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

    {-# INLINE fromInteger #-}
    fromInteger (S# i#)    = case (int2Float# i#) of { d# -> F# d# }
    fromInteger (J# s# d#) = encodeFloat# s# d# 0
	-- previous code: fromInteger n	= encodeFloat n 0
	-- doesn't work too well, because encodeFloat is defined in
	-- terms of ccalls which can never be simplified away.  We
	-- want simple literals like (fromInteger 3 :: Float) to turn
	-- into (F# 3.0), hence the special case for S# here.

instance  Real Float  where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Float  where
    (/) x y		=  divideFloat x y
    fromRational x	=  fromRat x
    recip x		=  1.0 / x

{-# RULES "truncate/Float->Int" truncate = float2Int #-}
instance  RealFrac Float  where

    {-# SPECIALIZE properFraction :: Float -> (Int, Float) #-}
    {-# SPECIALIZE round    :: Float -> Int #-}

    {-# SPECIALIZE properFraction :: Float  -> (Integer, Float) #-}
    {-# SPECIALIZE round    :: Float -> Integer #-}

	-- ceiling, floor, and truncate are all small
    {-# INLINE ceiling #-}
    {-# INLINE floor #-}
    {-# INLINE truncate #-}

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

instance  RealFloat Float  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  FLT_MANT_DIG	    -- ditto
    floatRange _	=  (FLT_MIN_EXP, FLT_MAX_EXP) -- ditto

    decodeFloat (F# f#)
      = case decodeFloat# f#	of
	  (# exp#, s#, d# #) -> (J# s# d#, I# exp#)

    encodeFloat (S# i) j     = int_encodeFloat# i j
    encodeFloat (J# s# d#) e = encodeFloat# s# d# e

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)
    isNaN x          = 0 /= isFloatNaN x
    isInfinite x     = 0 /= isFloatInfinite x
    isDenormalized x = 0 /= isFloatDenormalized x
    isNegativeZero x = 0 /= isFloatNegativeZero x
    isIEEE _         = True

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

    {-# INLINE fromInteger #-}
	-- See comments with Num Float
    fromInteger (S# i#)    = case (int2Double# i#) of { d# -> D# d# }
    fromInteger (J# s# d#) = encodeDouble# s# d# 0


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

{-# RULES "truncate/Double->Int" truncate = double2Int #-}
instance  RealFrac Double  where

    {-# SPECIALIZE properFraction :: Double -> (Int, Double) #-}
    {-# SPECIALIZE round    :: Double -> Int #-}

    {-# SPECIALIZE properFraction :: Double -> (Integer, Double) #-}
    {-# SPECIALIZE round    :: Double -> Integer #-}

	-- ceiling, floor, and truncate are all small
    {-# INLINE ceiling #-}
    {-# INLINE floor #-}
    {-# INLINE truncate #-}

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

    decodeFloat (D# x#)
      = case decodeDouble# x#	of
	  (# exp#, s#, d# #) -> (J# s# d#, I# exp#)

    encodeFloat (S# i) j     = int_encodeDouble# i j
    encodeFloat (J# s# d#) e = encodeDouble# s# d# e

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

    isNaN x 		= 0 /= isDoubleNaN x
    isInfinite x 	= 0 /= isDoubleInfinite x
    isDenormalized x 	= 0 /= isDoubleDenormalized x
    isNegativeZero x 	= 0 /= isDoubleNegativeZero x
    isIEEE _    	= True

instance  Show Double  where
    showsPrec   x = showSigned showFloat x
    showList = showList__ (showsPrec 0) 
\end{code}

%*********************************************************
%*							*
\subsection{@Enum@ instances}
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
    succ x	   = x + 1
    pred x	   = x - 1
    toEnum         = int2Float
    fromEnum       = fromInteger . truncate   -- may overflow
    enumFrom	   = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

instance  Enum Double  where
    succ x	   = x + 1
    pred x	   = x - 1
    toEnum         =  int2Double
    fromEnum       =  fromInteger . truncate   -- may overflow
    enumFrom	   =  numericEnumFrom
    enumFromTo     =  numericEnumFromTo
    enumFromThen   =  numericEnumFromThen
    enumFromThenTo =  numericEnumFromThenTo
\end{code}


%*********************************************************
%*							*
\subsection{Printing floating point}
%*							*
%*********************************************************


\begin{code}
-- | Show a signed 'RealFloat' value to full precision
-- using standard decimal notation for arguments whose absolute value lies 
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
showFloat :: (RealFloat a) => a -> ShowS
showFloat x  =  showString (formatRealFloat FFGeneric Nothing x)

-- These are the format types.  This type is not exported.

data FFFormat = FFExponent | FFFixed | FFGeneric

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x
   | isNaN x		       = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise		       = doFmt fmt (floatToDigits (toInteger base) x)
 where 
  base = 10

  doFmt format (is, e) =
    let ds = map intToDigit is in
    case format of
     FFGeneric ->
      doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
	    (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let show_e' = show (e-1) in
	case ds of
          "0"     -> "0.0e0"
          [d]     -> d : ".0e" ++ show_e'
	  (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
         _ ->
          let
	   (ei,is') = roundTo base (dec'+1) is
	   (d:ds') = map intToDigit (if ei > 0 then init is' else is')
          in
	  d:'.':ds' ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing
	  | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
	  | otherwise ->
	     let
	  	f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
	  	f n s    ""  = f (n-1) ('0':s) ""
	  	f n s (r:rs) = f (n-1) (r:s) rs
	     in
	 	f e "" ds
       Just dec ->
        let dec' = max dec 0 in
	if e >= 0 then
	 let
	  (ei,is') = roundTo base (dec' + e) is
	  (ls,rs)  = splitAt (e+ei) (map intToDigit is')
	 in
	 mk0 ls ++ (if null rs then "" else '.':rs)
	else
	 let
	  (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
	  d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
	 in
	 d : (if null ds' then "" else '.':ds')


roundTo :: Int -> Int -> [Int] -> (Int,[Int])
roundTo base d is =
  case f d is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
 where
  b2 = base `div` 2

  f n []     = (0, replicate n 0)
  f 0 (x:_)  = (if x >= b2 then 1 else 0, [])
  f n (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) xs
       i'     = c + i

-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R.K. Dybvig in PLDI 96.
-- This version uses a much slower logarithm estimator. It should be improved.

-- | 'floatToDigits' takes a base and a non-negative 'RealFloat' number,
-- and returns a list of digits and an exponent. 
-- In particular, if @x>=0@, and
--
-- > floatToDigits base x = ([d1,d2,...,dn], e)
--
-- then
--
--	(1) @n >= 1@
--
--	(2) @x = 0.d1d2...dn * (base**e)@
--
-- 	(3) @0 <= di <= base-1@

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
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
  k :: Int
  k =
   let 
    k0 :: Int
    k0 =
     if b == 2 && base == 10 then
        -- logBase 10 2 is slightly bigger than 3/10 so
	-- the following will err on the low side.  Ignoring
	-- the fraction will make it err even more.
	-- Haskell promises that p-1 <= logBase b f < p.
	(p - 1 + e0) * 3 `div` 10
     else
        ceiling ((log (fromInteger (f+1)) +
	         fromInteger (int2Integer e) * log (fromInteger b)) /
		   log (fromInteger base))
--WAS:		  fromInt e * log (fromInteger b))

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
 (map fromIntegral (reverse rds), k)

\end{code}


%*********************************************************
%*							*
\subsection{Converting from a Rational to a RealFloat
%*							*
%*********************************************************

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

Here's Joe's code:

\begin{pseudocode}
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

Now, here's Lennart's code (which works)

\begin{code}
-- | Converts a 'Rational' value into any type in class 'RealFloat'.
{-# SPECIALISE fromRat :: Rational -> Double,
			  Rational -> Float #-}
fromRat :: (RealFloat a) => Rational -> a

-- Deal with special cases first, delegating the real work to fromRat'
fromRat (n :% 0) | n > 0  =  1/0	-- +Infinity
		 | n == 0 =  0/0	-- NaN
		 | n < 0  = -1/0	-- -Infinity

fromRat (n :% d) | n > 0  = fromRat' (n :% d)
		 | n == 0 = encodeFloat 0 0 		-- Zero
		 | n < 0  = - fromRat' ((-n) :% d)

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.

fromRat' :: (RealFloat a) => Rational -> a
-- Invariant: argument is strictly positive
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
	(minExp0, _) = floatRange r
	minExp = minExp0 - p		-- the real minimum exponent
	xMin   = toRational (expt b (p-1))
	xMax   = toRational (expt b p)
	p0 = (integerLogBase b (numerator x) - integerLogBase b (denominator x) - p) `max` minExp
	f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
	(x', p') = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
	r = encodeFloat (round x') p'

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x 
 | p <= minExp = (x, p)
 | x >= xMax   = scaleRat b minExp xMin xMax (p+1) (x/b)
 | x < xMin    = scaleRat b minExp xMin xMax (p-1) (x*b)
 | otherwise   = (x, p)

-- Exponentiation with a cache for the most common numbers.
minExpt, maxExpt :: Int
minExpt = 0
maxExpt = 1100

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
integerLogBase b i
   | i < b     = 0
   | otherwise = doDiv (i `div` (b^l)) l
       where
	-- Try squaring the base first to cut down the number of divisions.
         l = 2 * integerLogBase (b*b) i

	 doDiv :: Integer -> Int -> Int
	 doDiv x y
	    | x < b     = y
	    | otherwise = doDiv (x `div` b) (y+1)

\end{code}


%*********************************************************
%*							*
\subsection{Floating point numeric primops}
%*							*
%*********************************************************

Definitions of the boxed PrimOps; these will be
used in the case of partial applications, etc.

\begin{code}
plusFloat, minusFloat, timesFloat, divideFloat :: Float -> Float -> Float
plusFloat   (F# x) (F# y) = F# (plusFloat# x y)
minusFloat  (F# x) (F# y) = F# (minusFloat# x y)
timesFloat  (F# x) (F# y) = F# (timesFloat# x y)
divideFloat (F# x) (F# y) = F# (divideFloat# x y)

negateFloat :: Float -> Float
negateFloat (F# x)        = F# (negateFloat# x)

gtFloat, geFloat, eqFloat, neFloat, ltFloat, leFloat :: Float -> Float -> Bool
gtFloat	    (F# x) (F# y) = gtFloat# x y
geFloat	    (F# x) (F# y) = geFloat# x y
eqFloat	    (F# x) (F# y) = eqFloat# x y
neFloat	    (F# x) (F# y) = neFloat# x y
ltFloat	    (F# x) (F# y) = ltFloat# x y
leFloat	    (F# x) (F# y) = leFloat# x y

float2Int :: Float -> Int
float2Int   (F# x) = I# (float2Int# x)

int2Float :: Int -> Float
int2Float   (I# x) = F# (int2Float# x)

expFloat, logFloat, sqrtFloat :: Float -> Float
sinFloat, cosFloat, tanFloat  :: Float -> Float
asinFloat, acosFloat, atanFloat  :: Float -> Float
sinhFloat, coshFloat, tanhFloat  :: Float -> Float
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

powerFloat :: Float -> Float -> Float
powerFloat  (F# x) (F# y) = F# (powerFloat# x y)

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusDouble, minusDouble, timesDouble, divideDouble :: Double -> Double -> Double
plusDouble   (D# x) (D# y) = D# (x +## y)
minusDouble  (D# x) (D# y) = D# (x -## y)
timesDouble  (D# x) (D# y) = D# (x *## y)
divideDouble (D# x) (D# y) = D# (x /## y)

negateDouble :: Double -> Double
negateDouble (D# x)        = D# (negateDouble# x)

gtDouble, geDouble, eqDouble, neDouble, leDouble, ltDouble :: Double -> Double -> Bool
gtDouble    (D# x) (D# y) = x >## y
geDouble    (D# x) (D# y) = x >=## y
eqDouble    (D# x) (D# y) = x ==## y
neDouble    (D# x) (D# y) = x /=## y
ltDouble    (D# x) (D# y) = x <## y
leDouble    (D# x) (D# y) = x <=## y

double2Int :: Double -> Int
double2Int   (D# x) = I# (double2Int#   x)

int2Double :: Int -> Double
int2Double   (I# x) = D# (int2Double#   x)

double2Float :: Double -> Float
double2Float (D# x) = F# (double2Float# x)

float2Double :: Float -> Double
float2Double (F# x) = D# (float2Double# x)

expDouble, logDouble, sqrtDouble :: Double -> Double
sinDouble, cosDouble, tanDouble  :: Double -> Double
asinDouble, acosDouble, atanDouble  :: Double -> Double
sinhDouble, coshDouble, tanhDouble  :: Double -> Double
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

powerDouble :: Double -> Double -> Double
powerDouble  (D# x) (D# y) = D# (x **## y)
\end{code}

\begin{code}
foreign import ccall unsafe "__encodeFloat"
	encodeFloat# :: Int# -> ByteArray# -> Int -> Float
foreign import ccall unsafe "__int_encodeFloat"
	int_encodeFloat# :: Int# -> Int -> Float


foreign import ccall unsafe "isFloatNaN" isFloatNaN :: Float -> Int
foreign import ccall unsafe "isFloatInfinite" isFloatInfinite :: Float -> Int
foreign import ccall unsafe "isFloatDenormalized" isFloatDenormalized :: Float -> Int
foreign import ccall unsafe "isFloatNegativeZero" isFloatNegativeZero :: Float -> Int


foreign import ccall unsafe "__encodeDouble"
	encodeDouble# :: Int# -> ByteArray# -> Int -> Double
foreign import ccall unsafe "__int_encodeDouble"
	int_encodeDouble# :: Int# -> Int -> Double

foreign import ccall unsafe "isDoubleNaN" isDoubleNaN :: Double -> Int
foreign import ccall unsafe "isDoubleInfinite" isDoubleInfinite :: Double -> Int
foreign import ccall unsafe "isDoubleDenormalized" isDoubleDenormalized :: Double -> Int
foreign import ccall unsafe "isDoubleNegativeZero" isDoubleNegativeZero :: Double -> Int
\end{code}

%*********************************************************
%*							*
\subsection{Coercion rules}
%*							*
%*********************************************************

\begin{code}
{-# RULES
"fromIntegral/Int->Float"   fromIntegral = int2Float
"fromIntegral/Int->Double"  fromIntegral = int2Double
"realToFrac/Float->Float"   realToFrac   = id :: Float -> Float
"realToFrac/Float->Double"  realToFrac   = float2Double
"realToFrac/Double->Float"  realToFrac   = double2Float
"realToFrac/Double->Double" realToFrac   = id :: Double -> Double
    #-}
\end{code}
