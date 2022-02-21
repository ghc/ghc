{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , GHCForeignImportPrim
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes
  #-}
{-# LANGUAGE CApiFFI #-}
-- We believe we could deorphan this module, by moving lots of things
-- around, but we haven't got there yet:
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Float
-- Copyright   :  (c) The University of Glasgow 1994-2002
--                Portions obtained from hbc (c) Lennart Augusstson
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Float' and 'Double', the classes 'Floating' and 'RealFloat' and
-- casting between Word32 and Float and Word64 and Double.
--
-----------------------------------------------------------------------------

#include "ieee-flpt.h"
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 32
# define WSHIFT 5
# define MMASK 31
#elif WORD_SIZE_IN_BITS == 64
# define WSHIFT 6
# define MMASK 63
#else
# error unsupported WORD_SIZE_IN_BITS
#endif


module GHC.Float
   ( module GHC.Float
   , Float(..), Double(..), Float#, Double#
   , double2Int, int2Double, float2Int, int2Float

    -- * Monomorphic equality operators
    -- | See GHC.Classes#matching_overloaded_methods_in_rules
   , eqFloat, eqDouble
   ) where

import Data.Maybe

import GHC.Base
import GHC.Bits
import GHC.List
import GHC.Enum
import GHC.Show
import GHC.Num
import GHC.Real
import GHC.Word
import GHC.Arr
import GHC.Float.RealFracMethods
import GHC.Float.ConversionUtils
import GHC.Num.BigNat

infixr 8  **

-- $setup
-- >>> import Prelude

------------------------------------------------------------------------
-- Standard numeric classes
------------------------------------------------------------------------

-- | Trigonometric and hyperbolic functions and related functions.
--
-- The Haskell Report defines no laws for 'Floating'. However, @('+')@, @('*')@
-- and 'exp' are customarily expected to define an exponential field and have
-- the following properties:
--
-- * @exp (a + b)@ = @exp a * exp b@
-- * @exp (fromInteger 0)@ = @fromInteger 1@
--
class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    -- | @'log1p' x@ computes @'log' (1 + x)@, but provides more precise
    -- results for small (absolute) values of @x@ if possible.
    --
    -- @since 4.9.0.0
    log1p               :: a -> a

    -- | @'expm1' x@ computes @'exp' x - 1@, but provides more precise
    -- results for small (absolute) values of @x@ if possible.
    --
    -- @since 4.9.0.0
    expm1               :: a -> a

    -- | @'log1pexp' x@ computes @'log' (1 + 'exp' x)@, but provides more
    -- precise results if possible.
    --
    -- Examples:
    --
    -- * if @x@ is a large negative number, @'log' (1 + 'exp' x)@ will be
    --   imprecise for the reasons given in 'log1p'.
    --
    -- * if @'exp' x@ is close to @-1@, @'log' (1 + 'exp' x)@ will be
    --   imprecise for the reasons given in 'expm1'.
    --
    -- @since 4.9.0.0
    log1pexp            :: a -> a

    -- | @'log1mexp' x@ computes @'log' (1 - 'exp' x)@, but provides more
    -- precise results if possible.
    --
    -- Examples:
    --
    -- * if @x@ is a large negative number, @'log' (1 - 'exp' x)@ will be
    --   imprecise for the reasons given in 'log1p'.
    --
    -- * if @'exp' x@ is close to @1@, @'log' (1 - 'exp' x)@ will be
    --   imprecise for the reasons given in 'expm1'.
    --
    -- @since 4.9.0.0
    log1mexp            :: a -> a

    {-# INLINE (**) #-}
    {-# INLINE logBase #-}
    {-# INLINE sqrt #-}
    {-# INLINE tan #-}
    {-# INLINE tanh #-}
    x ** y              =  exp (log x * y)
    logBase x y         =  log y / log x
    sqrt x              =  x ** 0.5
    tan  x              =  sin  x / cos  x
    tanh x              =  sinh x / cosh x

    {-# INLINE log1p #-}
    {-# INLINE expm1 #-}
    {-# INLINE log1pexp #-}
    {-# INLINE log1mexp #-}
    log1p x = log (1 + x)
    expm1 x = exp x - 1
    log1pexp x = log1p (exp x)
    log1mexp x = log1p (negate (exp x))

-- | Default implementation for @'log1mexp'@ requiring @'Ord'@ to test
-- against a threshold to decide which implementation variant to use.
log1mexpOrd :: (Ord a, Floating a) => a -> a
{-# INLINE log1mexpOrd #-}
log1mexpOrd a
    | a > -(log 2) = log (negate (expm1 a))
    | otherwise  = log1p (negate (exp a))

-- | Efficient, machine-independent access to the components of a
-- floating-point number.
class  (RealFrac a, Floating a) => RealFloat a  where
    -- | a constant function, returning the radix of the representation
    -- (often @2@)
    floatRadix          :: a -> Integer
    -- | a constant function, returning the number of digits of
    -- 'floatRadix' in the significand
    floatDigits         :: a -> Int
    -- | a constant function, returning the lowest and highest values
    -- the exponent may assume
    floatRange          :: a -> (Int,Int)
    -- | The function 'decodeFloat' applied to a real floating-point
    -- number returns the significand expressed as an 'Integer' and an
    -- appropriately scaled exponent (an 'Int').  If @'decodeFloat' x@
    -- yields @(m,n)@, then @x@ is equal in value to @m*b^^n@, where @b@
    -- is the floating-point radix, and furthermore, either @m@ and @n@
    -- are both zero or else @b^(d-1) <= 'abs' m < b^d@, where @d@ is
    -- the value of @'floatDigits' x@.
    -- In particular, @'decodeFloat' 0 = (0,0)@. If the type
    -- contains a negative zero, also @'decodeFloat' (-0.0) = (0,0)@.
    -- /The result of/ @'decodeFloat' x@ /is unspecified if either of/
    -- @'isNaN' x@ /or/ @'isInfinite' x@ /is/ 'True'.
    decodeFloat         :: a -> (Integer,Int)
    -- | 'encodeFloat' performs the inverse of 'decodeFloat' in the
    -- sense that for finite @x@ with the exception of @-0.0@,
    -- @'Prelude.uncurry' 'encodeFloat' ('decodeFloat' x) = x@.
    -- @'encodeFloat' m n@ is one of the two closest representable
    -- floating-point numbers to @m*b^^n@ (or @&#177;Infinity@ if overflow
    -- occurs); usually the closer, but if @m@ contains too many bits,
    -- the result may be rounded in the wrong direction.
    encodeFloat         :: Integer -> Int -> a
    -- | 'exponent' corresponds to the second component of 'decodeFloat'.
    -- @'exponent' 0 = 0@ and for finite nonzero @x@,
    -- @'exponent' x = snd ('decodeFloat' x) + 'floatDigits' x@.
    -- If @x@ is a finite floating-point number, it is equal in value to
    -- @'significand' x * b ^^ 'exponent' x@, where @b@ is the
    -- floating-point radix.
    -- The behaviour is unspecified on infinite or @NaN@ values.
    exponent            :: a -> Int
    -- | The first component of 'decodeFloat', scaled to lie in the open
    -- interval (@-1@,@1@), either @0.0@ or of absolute value @>= 1\/b@,
    -- where @b@ is the floating-point radix.
    -- The behaviour is unspecified on infinite or @NaN@ values.
    significand         :: a -> a
    -- | multiplies a floating-point number by an integer power of the radix
    scaleFloat          :: Int -> a -> a
    -- | 'True' if the argument is an IEEE \"not-a-number\" (NaN) value
    isNaN               :: a -> Bool
    -- | 'True' if the argument is an IEEE infinity or negative infinity
    isInfinite          :: a -> Bool
    -- | 'True' if the argument is too small to be represented in
    -- normalized format
    isDenormalized      :: a -> Bool
    -- | 'True' if the argument is an IEEE negative zero
    isNegativeZero      :: a -> Bool
    -- | 'True' if the argument is an IEEE floating point number
    isIEEE              :: a -> Bool
    -- | a version of arctangent taking two real floating-point arguments.
    -- For real floating @x@ and @y@, @'atan2' y x@ computes the angle
    -- (from the positive x-axis) of the vector from the origin to the
    -- point @(x,y)@.  @'atan2' y x@ returns a value in the range [@-pi@,
    -- @pi@].  It follows the Common Lisp semantics for the origin when
    -- signed zeroes are supported.  @'atan2' y 1@, with @y@ in a type
    -- that is 'RealFloat', should return the same value as @'atan' y@.
    -- A default definition of 'atan2' is provided, but implementors
    -- can provide a more accurate implementation.
    atan2               :: a -> a -> a


    exponent x          =  if m == 0 then 0 else n + floatDigits x
                           where (m,n) = decodeFloat x

    significand x       =  encodeFloat m (negate (floatDigits x))
                           where (m,_) = decodeFloat x

    scaleFloat 0 x      =  x
    scaleFloat k x
      | isFix           =  x
      | otherwise       =  encodeFloat m (n + clamp b k)
                           where (m,n) = decodeFloat x
                                 (l,h) = floatRange x
                                 d     = floatDigits x
                                 b     = h - l + 4*d
                                 -- n+k may overflow, which would lead
                                 -- to wrong results, hence we clamp the
                                 -- scaling parameter.
                                 -- If n + k would be larger than h,
                                 -- n + clamp b k must be too, similar
                                 -- for smaller than l - d.
                                 -- Add a little extra to keep clear
                                 -- from the boundary cases.
                                 isFix = x == 0 || isNaN x || isInfinite x

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

------------------------------------------------------------------------
-- Float
------------------------------------------------------------------------

-- | @since 2.01
-- Note that due to the presence of @NaN@, not all elements of 'Float' have an
-- additive inverse.
--
-- >>> 0/0 + (negate 0/0 :: Float)
-- NaN
--
-- Also note that due to the presence of -0, `Float`'s 'Num' instance doesn't
-- have an additive identity
--
-- >>> 0 + (-0 :: Float)
-- 0.0
instance Num Float where
    (+)         x y     =  plusFloat x y
    (-)         x y     =  minusFloat x y
    negate      x       =  negateFloat x
    (*)         x y     =  timesFloat x y
    abs         x       =  fabsFloat x
    signum x | x > 0     = 1
             | x < 0     = negateFloat 1
             | otherwise = x -- handles 0.0, (-0.0), and NaN

    {-# INLINE fromInteger #-}
    fromInteger i = F# (integerToFloat# i)

-- | Convert an Integer to a Float#
integerToFloat# :: Integer -> Float#
{-# NOINLINE integerToFloat# #-}
integerToFloat# (IS i)   = int2Float# i
integerToFloat# i@(IP _) = case integerToBinaryFloat' i of
                             F# x -> x
integerToFloat# (IN bn)  = case integerToBinaryFloat' (IP bn) of
                             F# x -> negateFloat# x

-- | Convert a Natural to a Float#
naturalToFloat# :: Natural -> Float#
{-# NOINLINE naturalToFloat# #-}
naturalToFloat# (NS w) = word2Float# w
naturalToFloat# (NB b) = case integerToBinaryFloat' (IP b) of
                           F# x -> x

-- | @since 2.01
instance  Real Float  where
    toRational (F# x#)  =
        case decodeFloat_Int# x# of
          (# m#, e# #)
            | isTrue# (e# >=# 0#)                               ->
                    (IS m# `integerShiftL#` int2Word# e#) :% 1
            | isTrue# ((int2Word# m# `and#` 1##) `eqWord#` 0##) ->
                    case elimZerosInt# m# (negateInt# e#) of
                      (# n, d# #) -> n :% integerShiftL# 1 (int2Word# d#)
            | otherwise                                         ->
                    IS m# :% integerShiftL# 1 (int2Word# (negateInt# e#))

-- | @since 2.01
-- Note that due to the presence of @NaN@, not all elements of 'Float' have an
-- multiplicative inverse.
--
-- >>> 0/0 * (recip 0/0 :: Float)
-- NaN
instance  Fractional Float  where
    (/) x y             =  divideFloat x y
    {-# INLINE fromRational #-}
    fromRational (n:%d) = rationalToFloat n d
    recip x             =  1.0 / x

rationalToFloat :: Integer -> Integer -> Float
{-# NOINLINE [1] rationalToFloat #-}
rationalToFloat n 0
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToFloat n d
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = FLT_MIN_EXP
        mantDigs    = FLT_MANT_DIG

-- | @since 2.01
instance  RealFrac Float  where

   properFraction = properFractionFloat
   truncate       = truncateFloat
   round          = roundFloat
   floor          = floorFloat
   ceiling        = ceilingFloat

-- RULES for Integer and Int
-- Note [Rules for overloaded class methods]
{-# RULES
"properFraction/Float->Integer"     properFractionFloat = properFractionFloatInteger
"truncate/Float->Integer"           truncateFloat = truncateFloatInteger
"floor/Float->Integer"              floorFloat = floorFloatInteger
"ceiling/Float->Integer"            ceilingFloat = ceilingFloatInteger
"round/Float->Integer"              roundFloat = roundFloatInteger
"properFraction/Float->Int"         properFractionFloat = properFractionFloatInt
"truncate/Float->Int"               truncateFloat = float2Int
"floor/Float->Int"                  floorFloat = floorFloatInt
"ceiling/Float->Int"                ceilingFloat = ceilingFloatInt
"round/Float->Int"                  roundFloat = roundFloatInt
  #-}


floorFloat :: Integral b => Float -> b
{-# INLINE [1] floorFloat #-}
floorFloat x = case properFractionFloat x of
                    (n,r) -> if r < 0.0 then n - 1 else n

ceilingFloat :: Integral b => Float -> b
{-# INLINE [1] ceilingFloat #-}
ceilingFloat x = case properFraction x of
                    (n,r) -> if r > 0.0 then n + 1 else n

truncateFloat :: Integral b => Float -> b
{-# INLINE [1] truncateFloat #-}
truncateFloat x = case properFractionFloat x of
                     (n,_) -> n

roundFloat :: Integral b => Float -> b
{-# NOINLINE [1] roundFloat #-}
roundFloat x = case properFractionFloat x of
                     (n,r) -> let
                                m         = if r < 0.0 then n - 1 else n + 1
                                half_down = abs r - 0.5
                              in
                              case (compare half_down 0.0) of
                                LT -> n
                                EQ -> if even n then n else m
                                GT -> m

properFractionFloat :: Integral b => Float -> (b,Float)
{-# NOINLINE [1] properFractionFloat #-}

-- We assume that FLT_RADIX is 2 so that we can use more efficient code
#if FLT_RADIX != 2
#error FLT_RADIX must be 2
#endif
properFractionFloat (F# x#)
      = case decodeFloat_Int# x# of
        (# m#, n# #) ->
            let m = I# m#
                n = I# n#
            in
            if n >= 0
            then (fromIntegral m * (2 ^ n), 0.0)
            else let i = if m >= 0 then                m `shiftR` negate n
                                   else negate (negate m `shiftR` negate n)
                     f = m - (i `shiftL` negate n)
                 in (fromIntegral i, encodeFloat (fromIntegral f) n)



-- | @since 2.01
instance  Floating Float  where
    pi                  =  3.141592653589793238
    exp x               =  expFloat x
    log x               =  logFloat x
    sqrt x              =  sqrtFloat x
    sin x               =  sinFloat x
    cos x               =  cosFloat x
    tan x               =  tanFloat x
    asin x              =  asinFloat x
    acos x              =  acosFloat x
    atan x              =  atanFloat x
    sinh x              =  sinhFloat x
    cosh x              =  coshFloat x
    tanh x              =  tanhFloat x
    (**) x y            =  powerFloat x y
    logBase x y         =  log y / log x

    asinh x             =  asinhFloat x
    acosh x             =  acoshFloat x
    atanh x             =  atanhFloat x

    log1p = log1pFloat
    expm1 = expm1Float

    log1mexp x = log1mexpOrd x
    {-# INLINE log1mexp #-}
    log1pexp a
      | a <= 18   = log1pFloat (exp a)
      | a <= 100  = a + exp (negate a)
      | otherwise = a
    {-# INLINE log1pexp #-}

-- | @since 2.01
instance  RealFloat Float  where
    floatRadix _        =  FLT_RADIX        -- from float.h
    floatDigits _       =  FLT_MANT_DIG     -- ditto
    floatRange _        =  (FLT_MIN_EXP, FLT_MAX_EXP) -- ditto

    decodeFloat (F# f#) = case decodeFloat_Int# f# of
                          (# i, e #) -> (IS i, I# e)

    encodeFloat i (I# e) = F# (integerEncodeFloat# i e)

    exponent x          = case decodeFloat x of
                            (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x       = case decodeFloat x of
                            (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat 0 x      = x
    scaleFloat k x
      | isFix           = x
      | otherwise       = case decodeFloat x of
                            (m,n) -> encodeFloat m (n + clamp bf k)
                        where bf = FLT_MAX_EXP - (FLT_MIN_EXP) + 4*FLT_MANT_DIG
                              isFix = x == 0 || isFloatFinite x == 0

    isNaN x          = 0 /= isFloatNaN x
    isInfinite x     = 0 /= isFloatInfinite x
    isDenormalized x = 0 /= isFloatDenormalized x
    isNegativeZero x = 0 /= isFloatNegativeZero x
    isIEEE _         = True

-- | @since 2.01
instance  Show Float  where
    showsPrec   x = showSignedFloat showFloat x
    showList = showList__ (showsPrec 0)

------------------------------------------------------------------------
-- Double
------------------------------------------------------------------------

-- | @since 2.01
-- Note that due to the presence of @NaN@, not all elements of 'Double' have an
-- additive inverse.
--
-- >>> 0/0 + (negate 0/0 :: Double)
-- NaN
--
-- Also note that due to the presence of -0, `Double`'s 'Num' instance doesn't
-- have an additive identity
--
-- >>> 0 + (-0 :: Double)
-- 0.0
instance  Num Double  where
    (+)         x y     =  plusDouble x y
    (-)         x y     =  minusDouble x y
    negate      x       =  negateDouble x
    (*)         x y     =  timesDouble x y
    abs         x       =  fabsDouble x
    signum x | x > 0     = 1
             | x < 0     = negateDouble 1
             | otherwise = x -- handles 0.0, (-0.0), and NaN


    {-# INLINE fromInteger #-}
    fromInteger i = D# (integerToDouble# i)

-- | Convert an Integer to a Double#
integerToDouble# :: Integer -> Double#
{-# NOINLINE integerToDouble# #-}
integerToDouble# (IS i)   = int2Double# i
integerToDouble# i@(IP _) = case integerToBinaryFloat' i of
                              D# x -> x
integerToDouble# (IN bn)  = case integerToBinaryFloat' (IP bn) of
                              D# x -> negateDouble# x

-- | Encode a Natural (mantissa) into a Double#
naturalToDouble# :: Natural -> Double#
{-# NOINLINE naturalToDouble# #-}
naturalToDouble# (NS w) = word2Double# w
naturalToDouble# (NB b) = case integerToBinaryFloat' (IP b) of
                            D# x -> x


-- | @since 2.01
instance  Real Double  where
    toRational (D# x#)  =
        case integerDecodeDouble# x# of
          (# m, e# #)
            | isTrue# (e# >=# 0#)                                  ->
                integerShiftL# m (int2Word# e#) :% 1
            | isTrue# ((integerToWord# m `and#` 1##) `eqWord#` 0##) ->
                case elimZerosInteger m (negateInt# e#) of
                    (# n, d# #) ->  n :% integerShiftL# 1 (int2Word# d#)
            | otherwise                                            ->
                m :% integerShiftL# 1 (int2Word# (negateInt# e#))

-- | @since 2.01
-- Note that due to the presence of @NaN@, not all elements of 'Double' have an
-- multiplicative inverse.
--
-- >>> 0/0 * (recip 0/0 :: Double)
-- NaN
instance  Fractional Double  where
    (/) x y             =  divideDouble x y
    {-# INLINE fromRational #-}
    fromRational (n:%d) = rationalToDouble n d
    recip x             =  1.0 / x

rationalToDouble :: Integer -> Integer -> Double
{-# NOINLINE [1] rationalToDouble #-}
rationalToDouble n 0
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToDouble n d
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = DBL_MIN_EXP
        mantDigs    = DBL_MANT_DIG

-- | @since 2.01
instance  Floating Double  where
    pi                  =  3.141592653589793238
    exp x               =  expDouble x
    log x               =  logDouble x
    sqrt x              =  sqrtDouble x
    sin  x              =  sinDouble x
    cos  x              =  cosDouble x
    tan  x              =  tanDouble x
    asin x              =  asinDouble x
    acos x              =  acosDouble x
    atan x              =  atanDouble x
    sinh x              =  sinhDouble x
    cosh x              =  coshDouble x
    tanh x              =  tanhDouble x
    (**) x y            =  powerDouble x y
    logBase x y         =  log y / log x

    asinh x             =  asinhDouble x
    acosh x             =  acoshDouble x
    atanh x             =  atanhDouble x

    log1p = log1pDouble
    expm1 = expm1Double

    log1mexp x = log1mexpOrd x
    {-# INLINE log1mexp #-}
    log1pexp a
      | a <= 18   = log1pDouble (exp a)
      | a <= 100  = a + exp (negate a)
      | otherwise = a
    {-# INLINE log1pexp #-}

-- | @since 2.01
instance  RealFrac Double  where
    properFraction = properFractionDouble
    truncate       = truncateDouble
    round          = roundDouble
    ceiling        = ceilingDouble
    floor          = floorDouble

-- RULES for Integer and Int
-- Note [Rules for overloaded class methods]
{-# RULES
"properFraction/Double->Integer"    properFractionDouble = properFractionDoubleInteger
"truncate/Double->Integer"          truncateDouble = truncateDoubleInteger
"floor/Double->Integer"             floorDouble = floorDoubleInteger
"ceiling/Double->Integer"           ceilingDouble = ceilingDoubleInteger
"round/Double->Integer"             roundDouble = roundDoubleInteger
"properFraction/Double->Int"        properFractionDouble = properFractionDoubleInt
"truncate/Double->Int"              truncateDouble = double2Int
"floor/Double->Int"                 floorDouble = floorDoubleInt
"ceiling/Double->Int"               ceilingDouble = ceilingDoubleInt
"round/Double->Int"                 roundDouble = roundDoubleInt
  #-}

floorDouble :: Integral b => Double -> b
{-# INLINE [1] floorDouble #-}
floorDouble x = case properFractionDouble x of
                    (n,r) -> if r < 0.0 then n - 1 else n

ceilingDouble :: Integral b => Double -> b
{-# INLINE [1] ceilingDouble #-}
ceilingDouble x = case properFractionDouble x of
                    (n,r) -> if r > 0.0 then n + 1 else n

truncateDouble :: Integral b => Double -> b
{-# INLINE [1] truncateDouble #-}
truncateDouble x = case properFractionDouble x of
                     (n,_) -> n

roundDouble :: Integral b => Double -> b
{-# NOINLINE [1] roundDouble #-}
roundDouble x
  = case properFractionDouble x of
      (n,r) -> let
                 m         = if r < 0.0 then n - 1 else n + 1
                 half_down = abs r - 0.5
               in
               case (compare half_down 0.0) of
                 LT -> n
                 EQ -> if even n then n else m
                 GT -> m

properFractionDouble :: Integral b => Double -> (b,Double)
{-# NOINLINE [1] properFractionDouble #-}
properFractionDouble x
  = case (decodeFloat x)      of { (m,n) ->
    if n >= 0 then
        (fromInteger m * 2 ^ n, 0.0)
    else
        case (quotRem m (2^(negate n))) of { (w,r) ->
        (fromInteger w, encodeFloat r n)
        }
    }

{- Note [Rules for overloaded class methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a method like
    class ... => RealFrac a where
       floor :: Integral b => a -> b

So floor :: forall a. RealFrac a => forall b. Integral b => a -> b

Now suppose we want to hand-write specialised code for
(floor @Double d1 @Int d2). We used to do this:
   RULE "floor/Double->Int" floor = floorDoubleInt
where GHC.Float.RealFracMethods defines
   floorDoubleInt :: Double -> Int

This RULE in full is
   RULE "floor/Double->Int" forall d1 d2.
         floor @Double d1 @Int d2 = floorDoubleInt

But it's best not to write one rule for this entire step. I found cases
(in nofib/real/gamteb/Utils.hs, function fiRem) where we floated
out the (floor @Double d1) part, so the above two-argument rule didn't fire.
Instead the class-op rule fired.  Boo!  See #19582.

Best to define an instance
   instance RealFrac Double where
      floor = floorDouble

   floorDouble :: Integral b => Double -> b
   floorDouble = <code for floorDouble>
   {-# RULE "floorDouble/Int" floorDouble @Int d = floorDoubleInt #-}

Now we rewrite as follows
   floor @Double d1 @Int d2
   --> { ClassOp rule for floor }
       floorDouble @Int d2
   --> { Hand-written RULE "floorDouble/Int" }
       floorDoubleInt

More robust!  This pattern applies for any class method that
has local overloading, in particular:
  * properFraction
  * truncate
  * floor
  * ceiling
  * round

All of this is really stated, in more general form, in the GHC
user manual section "How rules interact with class methods".
-}

-- | @since 2.01
instance  RealFloat Double  where
    floatRadix _        =  FLT_RADIX        -- from float.h
    floatDigits _       =  DBL_MANT_DIG     -- ditto
    floatRange _        =  (DBL_MIN_EXP, DBL_MAX_EXP) -- ditto

    decodeFloat (D# x#)
      = case integerDecodeDouble# x#   of
          (# i, j #) -> (i, I# j)

    encodeFloat i (I# j) = D# (integerEncodeDouble# i j)

    exponent x          = case decodeFloat x of
                            (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x       = case decodeFloat x of
                            (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat 0 x      = x
    scaleFloat k x
      | isFix           = x
      | otherwise       = case decodeFloat x of
                            (m,n) -> encodeFloat m (n + clamp bd k)
                        where bd = DBL_MAX_EXP - (DBL_MIN_EXP) + 4*DBL_MANT_DIG
                              isFix = x == 0 || isDoubleFinite x == 0

    isNaN x             = 0 /= isDoubleNaN x
    isInfinite x        = 0 /= isDoubleInfinite x
    isDenormalized x    = 0 /= isDoubleDenormalized x
    isNegativeZero x    = 0 /= isDoubleNegativeZero x
    isIEEE _            = True

-- | @since 2.01
instance  Show Double  where
    showsPrec   x = showSignedFloat showFloat x
    showList = showList__ (showsPrec 0)


------------------------------------------------------------------------
-- Enum instances
------------------------------------------------------------------------

{-
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
-}

-- | @since 2.01
instance  Enum Float  where
    succ x         = x + 1
    pred x         = x - 1
    toEnum         = int2Float
    fromEnum       = fromInteger . truncate   -- may overflow
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

-- | @since 2.01
instance  Enum Double  where
    succ x         = x + 1
    pred x         = x - 1
    toEnum         =  int2Double
    fromEnum       =  fromInteger . truncate   -- may overflow
    enumFrom       =  numericEnumFrom
    enumFromTo     =  numericEnumFromTo
    enumFromThen   =  numericEnumFromThen
    enumFromThenTo =  numericEnumFromThenTo

------------------------------------------------------------------------
-- Printing floating point
------------------------------------------------------------------------

-- | Show a signed 'RealFloat' value to full precision
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
showFloat :: (RealFloat a) => a -> ShowS
showFloat x  =  showString (formatRealFloat FFGeneric Nothing x)

-- These are the format types.  This type is not exported.

data FFFormat = FFExponent | FFFixed | FFGeneric

-- This is just a compatibility stub, as the "alt" argument formerly
-- didn't exist.
formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = formatRealFloatAlt fmt decs False x

formatRealFloatAlt :: (RealFloat a) => FFFormat -> Maybe Int -> Bool -> a
                 -> String
formatRealFloatAlt fmt decs alt x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt fmt (floatToDigits (toInteger base) x)
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
          []      -> errorWithoutStackTrace "formatRealFloat/doFmt/FFExponent: []"
       Just d | d <= 0 ->
        -- handle this case specifically since we need to omit the
        -- decimal point as well (#15115).
        -- Note that this handles negative precisions as well for consistency
        -- (see #15509).
        case is of
          [0] -> "0e0"
          _ ->
           let
             (ei,is') = roundTo base 1 is
             n:_ = map intToDigit (if ei > 0 then init is' else is')
           in n : 'e' : show (e-1+ei)
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
         mk0 ls ++ (if null rs && not alt then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' && not alt then "" else '.':ds')


roundTo :: Int -> Int -> [Int] -> (Int,[Int])
roundTo base d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> errorWithoutStackTrace "roundTo: bad Value"
 where
  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
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
--      (1) @n >= 1@
--
--      (2) @x = 0.d1d2...dn * (base**e)@
--
--      (3) @0 <= di <= base-1@

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
   if n > 0 then (f0 `quot` (expt b n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = expt b e in
    if f == expt b (p-1) then
      (f*be*b*2, 2*b, be*b, be)     -- according to Burger and Dybvig
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == expt b (p-1) then
      (f*b*2, expt b (-e+1)*2, b, 1)
    else
      (f*2, expt b (-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 && base == 10 then
        -- logBase 10 2 is very slightly larger than 8651/28738
        -- (about 5.3558e-10), so if log x >= 0, the approximation
        -- k1 is too small, hence we add one and need one fixup step less.
        -- If log x < 0, the approximation errs rather on the high side.
        -- That is usually more than compensated for by ignoring the
        -- fractional part of logBase 2 x, but when x is a power of 1/2
        -- or slightly larger and the exponent is a multiple of the
        -- denominator of the rational approximation to logBase 10 2,
        -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
        -- we get a leading zero-digit we don't want.
        -- With the approximation 3/10, this happened for
        -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
        -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
        -- for IEEE-ish floating point types with exponent fields
        -- <= 17 bits and mantissae of several thousand bits, earlier
        -- convergents to logBase 10 2 would fail for long double.
        -- Using quot instead of div is a little faster and requires
        -- fewer fixup steps for negative lx.
        let lx = p - 1 + e0
            k1 = (lx * 8651) `quot` 28738
        in if lx >= 0 then k1 + 1 else k1
     else
        -- f :: Integer, log :: Float -> Float,
        --               ceiling :: Float -> Int
        ceiling ((log (fromInteger (f+1) :: Float) +
                 fromIntegral e * log (fromInteger b)) /
                   log (fromInteger base))
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt base n * s then n else fixup (n+1)
      else
        if expt base (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * base) `quotRem` sN
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

------------------------------------------------------------------------
-- Converting from an Integer to a RealFloat
------------------------------------------------------------------------

{-# SPECIALISE integerToBinaryFloat' :: Integer -> Float,
                                        Integer -> Double #-}
-- | Converts a positive integer to a floating-point value.
--
-- The value nearest to the argument will be returned.
-- If there are two such values, the one with an even significand will
-- be returned (i.e. IEEE roundTiesToEven).
--
-- The argument must be strictly positive, and @floatRadix (undefined :: a)@ must be 2.
integerToBinaryFloat' :: RealFloat a => Integer -> a
integerToBinaryFloat' n = result
  where
    mantDigs = floatDigits result
    k = I# (word2Int# (integerLog2# n))
    result = if k < mantDigs then
               encodeFloat n 0
             else
               let !e@(I# e#) = k - mantDigs + 1
                   q = n `unsafeShiftR` e
                   n' = case roundingMode# n (e# -# 1#) of
                          0# -> q
                          1# -> if integerToInt q .&. 1 == 0 then
                                  q
                                else
                                  q + 1
                          _ {- 2# -} -> q + 1
               in encodeFloat n' e

------------------------------------------------------------------------
-- Converting from a Rational to a RealFloat
------------------------------------------------------------------------

{-
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

--              If the exponent of the nearest floating-point number to x
--              is e, then the significand is the integer nearest xb^(-e),
--              where b is the floating-point radix.  We start with a good
--              guess for e, and if it is correct, the exponent of the
--              floating-point number we construct will again be e.  If
--              not, one more iteration is needed.

              f e   = if e' == e then y else f e'
                      where y      = encodeFloat (round (x * (1 % b)^^e)) e
                            (_,e') = decodeFloat y
              b     = floatRadix x'

--              We obtain a trial exponent by doing a floating-point
--              division of x's numerator by its denominator.  The
--              result of this division may not itself be the ultimate
--              result, because of an accumulation of three rounding
--              errors.

              (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
                                        / fromInteger (denominator x))
\end{pseudocode}

Now, here's Lennart's code (which works):
-}

-- | Converts a 'Rational' value into any type in class 'RealFloat'.
{-# RULES
"fromRat/Float"     fromRat = (fromRational :: Rational -> Float)
"fromRat/Double"    fromRat = (fromRational :: Rational -> Double)
  #-}

{-# NOINLINE [2] fromRat #-}
-- See Note [Allow time for type-specialisation rules to fire] in GHC.Real
fromRat :: (RealFloat a) => Rational -> a

-- Deal with special cases first, delegating the real work to fromRat'
fromRat (n :% 0) | n > 0     =  1/0        -- +Infinity
                 | n < 0     = -1/0        -- -Infinity
                 | otherwise =  0/0        -- NaN

fromRat (n :% d) | n > 0     = fromRat' (n :% d)
                 | n < 0     = - fromRat' ((-n) :% d)
                 | otherwise = encodeFloat 0 0             -- Zero

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
        minExp = minExp0 - p            -- the real minimum exponent
        xMax   = toRational (expt b p)
        ln     = I# (word2Int# (integerLogBase# b (numerator x)))
        ld     = I# (word2Int# (integerLogBase# b (denominator x)))
        p0     = (ln - ld - p) `max` minExp
        -- if x = n/d and ln = integerLogBase b n, ld = integerLogBase b d,
        -- then b^(ln-ld-1) < x < b^(ln-ld+1)
        f = if p0 < 0 then 1 :% expt b (-p0) else expt b p0 :% 1
        x0 = x / f
        -- if ln - ld >= minExp0, then b^(p-1) < x0 < b^(p+1), so there's at most
        -- one scaling step needed, otherwise, x0 < b^p and no scaling is needed
        (x', p') = if x0 >= xMax then (x0 / toRational b, p0+1) else (x0, p0)
        r = encodeFloat (round x') p'

-- Exponentiation with a cache for the most common numbers.
minExpt, maxExpt :: Int
minExpt = 0
maxExpt = 1100

expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        if base == 10 && n <= maxExpt10 then
            expts10!n
        else
            base^n

expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

maxExpt10 :: Int
maxExpt10 = 324

expts10 :: Array Int Integer
expts10 = array (minExpt,maxExpt10) [(n,10^n) | n <- [minExpt .. maxExpt10]]

{-
Unfortunately, the old conversion code was awfully slow due to
a) a slow integer logarithm
b) repeated calculation of gcd's

For the case of Rational's coming from a Float or Double via toRational,
we can exploit the fact that the denominator is a power of two, which for
these brings a huge speedup since we need only shift and add instead
of division.

The below is an adaption of fromRat' for the conversion to
Float or Double exploiting the known floatRadix and avoiding
divisions as much as possible.
-}

{-# SPECIALISE fromRat'' :: Int -> Int -> Integer -> Integer -> Float,
                            Int -> Int -> Integer -> Integer -> Double #-}
fromRat'' :: RealFloat a => Int -> Int -> Integer -> Integer -> a
-- Invariant: n and d strictly positive
fromRat'' minEx@(I# me#) mantDigs@(I# md#) n d =
    case integerIsPowerOf2# d of
      (# | ldw# #) ->
          let ld# = word2Int# ldw#
          in case word2Int# (integerLog2# n) of
            ln# | isTrue# (ln# >=# (ld# +# me# -# 1#)) ->
                  -- this means n/d >= 2^(minEx-1), i.e. we are guaranteed to get
                  -- a normalised number, round to mantDigs bits
                  if isTrue# (ln# <# md#)
                    then encodeFloat n (I# (negateInt# ld#))
                    else let n'  = n `shiftR` (I# (ln# +# 1# -# md#))
                             n'' = case roundingMode# n (ln# -# md#) of
                                    0# -> n'
                                    2# -> n' + 1
                                    _  -> case fromInteger n' .&. (1 :: Int) of
                                            0 -> n'
                                            _ -> n' + 1
                         in encodeFloat n'' (I# (ln# -# ld# +# 1# -# md#))
                | otherwise ->
                  -- n/d < 2^(minEx-1), a denorm or rounded to 2^(minEx-1)
                  -- the exponent for encoding is always minEx-mantDigs
                  -- so we must shift right by (minEx-mantDigs) - (-ld)
                  case ld# +# (me# -# md#) of
                    ld'# | isTrue# (ld'# <=# 0#) -> -- we would shift left, so we don't shift
                           encodeFloat n (I# ((me# -# md#) -# ld'#))
                         | isTrue# (ld'# <=# ln#) ->
                           let n' = n `shiftR` (I# ld'#)
                           in case roundingMode# n (ld'# -# 1#) of
                                0# -> encodeFloat n' (minEx - mantDigs)
                                1# -> if fromInteger n' .&. (1 :: Int) == 0
                                        then encodeFloat n' (minEx-mantDigs)
                                        else encodeFloat (n' + 1) (minEx-mantDigs)
                                _  -> encodeFloat (n' + 1) (minEx-mantDigs)
                         | isTrue# (ld'# ># (ln# +# 1#)) -> encodeFloat 0 0 -- result of shift < 0.5
                         | otherwise ->  -- first bit of n shifted to 0.5 place
                           case integerIsPowerOf2# n of
                            (#       |  _ #) -> encodeFloat 0 0  -- round to even
                            (# (# #) |    #) -> encodeFloat 1 (minEx - mantDigs)
      (# (# #) | #) ->
          let ln = I# (word2Int# (integerLog2# n))
              ld = I# (word2Int# (integerLog2# d))
              -- 2^(ln-ld-1) < n/d < 2^(ln-ld+1)
              p0 = max minEx (ln - ld)
              (n', d')
                | p0 < mantDigs = (n `shiftL` (mantDigs - p0), d)
                | p0 == mantDigs = (n, d)
                | otherwise     = (n, d `shiftL` (p0 - mantDigs))
              -- if ln-ld < minEx, then n'/d' < 2^mantDigs, else
              -- 2^(mantDigs-1) < n'/d' < 2^(mantDigs+1) and we
              -- may need one scaling step
              scale p a b
                | (b `shiftL` mantDigs) <= a = (p+1, a, b `shiftL` 1)
                | otherwise = (p, a, b)
              (p', n'', d'') = scale (p0-mantDigs) n' d'
              -- n''/d'' < 2^mantDigs and p' == minEx-mantDigs or n''/d'' >= 2^(mantDigs-1)
              rdq = case n'' `quotRem` d'' of
                     (q,r) -> case compare (r `shiftL` 1) d'' of
                                LT -> q
                                EQ -> if fromInteger q .&. (1 :: Int) == 0
                                        then q else q+1
                                GT -> q+1
          in  encodeFloat rdq p'

-- Assumption: Integer and Int# are strictly positive, Int# is less
-- than logBase 2 of Integer, otherwise havoc ensues.
-- Used only for the numerator in fromRational when the denominator
-- is a power of 2.
-- The Int# argument is log2 n minus the number of bits in the mantissa
-- of the target type, i.e. the index of the first non-integral bit in
-- the quotient.
--
-- 0# means round down (towards zero)
-- 1# means we have a half-integer, round to even
-- 2# means round up (away from zero)
roundingMode# :: Integer -> Int# -> Int#
roundingMode# (IS i#) t =
   let
      k = int2Word# i# `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##)
      c = uncheckedShiftL# 1## t
   in if isTrue# (c `gtWord#` k)
         then 0#
         else if isTrue# (c `ltWord#` k)
                 then 2#
                 else 1#

roundingMode# (IN _)  _ = errorWithoutStackTrace "roundingMode#: IN" -- See the Assumption
roundingMode# (IP bn) t =
   let
      j = word2Int# (int2Word# t `and#` MMASK##) -- index of relevant bit in word
      k = uncheckedIShiftRA# t WSHIFT#           -- index of relevant word
      r = bigNatIndex# bn k `and#` ((uncheckedShiftL# 2## j) `minusWord#` 1##)
      c = uncheckedShiftL# 1## j
      test i = if isTrue# (i <# 0#)
                  then 1#
                  else case bigNatIndex# bn i of
                          0## -> test (i -# 1#)
                          _   -> 2#
   in if isTrue# (c `gtWord#` r)
         then 0#
         else if isTrue# (c `ltWord#` r)
                 then 2#
                 else test (k -# 1#)

------------------------------------------------------------------------
-- Floating point numeric primops
------------------------------------------------------------------------

-- Definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusFloat, minusFloat, timesFloat, divideFloat :: Float -> Float -> Float
plusFloat   (F# x) (F# y) = F# (plusFloat# x y)
minusFloat  (F# x) (F# y) = F# (minusFloat# x y)
timesFloat  (F# x) (F# y) = F# (timesFloat# x y)
divideFloat (F# x) (F# y) = F# (divideFloat# x y)

negateFloat :: Float -> Float
negateFloat (F# x)        = F# (negateFloat# x)

gtFloat, geFloat, ltFloat, leFloat :: Float -> Float -> Bool
gtFloat     (F# x) (F# y) = isTrue# (gtFloat# x y)
geFloat     (F# x) (F# y) = isTrue# (geFloat# x y)
ltFloat     (F# x) (F# y) = isTrue# (ltFloat# x y)
leFloat     (F# x) (F# y) = isTrue# (leFloat# x y)

expFloat, expm1Float :: Float -> Float
logFloat, log1pFloat, sqrtFloat, fabsFloat :: Float -> Float
sinFloat, cosFloat, tanFloat  :: Float -> Float
asinFloat, acosFloat, atanFloat  :: Float -> Float
sinhFloat, coshFloat, tanhFloat  :: Float -> Float
asinhFloat, acoshFloat, atanhFloat  :: Float -> Float
expFloat    (F# x) = F# (expFloat# x)
expm1Float  (F# x) = F# (expm1Float# x)
logFloat    (F# x) = F# (logFloat# x)
log1pFloat  (F# x) = F# (log1pFloat# x)
sqrtFloat   (F# x) = F# (sqrtFloat# x)
fabsFloat   (F# x) = F# (fabsFloat# x)
sinFloat    (F# x) = F# (sinFloat# x)
cosFloat    (F# x) = F# (cosFloat# x)
tanFloat    (F# x) = F# (tanFloat# x)
asinFloat   (F# x) = F# (asinFloat# x)
acosFloat   (F# x) = F# (acosFloat# x)
atanFloat   (F# x) = F# (atanFloat# x)
sinhFloat   (F# x) = F# (sinhFloat# x)
coshFloat   (F# x) = F# (coshFloat# x)
tanhFloat   (F# x) = F# (tanhFloat# x)
asinhFloat  (F# x) = F# (asinhFloat# x)
acoshFloat  (F# x) = F# (acoshFloat# x)
atanhFloat  (F# x) = F# (atanhFloat# x)

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

gtDouble, geDouble, leDouble, ltDouble :: Double -> Double -> Bool
gtDouble    (D# x) (D# y) = isTrue# (x >##  y)
geDouble    (D# x) (D# y) = isTrue# (x >=## y)
ltDouble    (D# x) (D# y) = isTrue# (x <##  y)
leDouble    (D# x) (D# y) = isTrue# (x <=## y)

double2Float :: Double -> Float
double2Float (D# x) = F# (double2Float# x)

float2Double :: Float -> Double
float2Double (F# x) = D# (float2Double# x)

expDouble, expm1Double :: Double -> Double
logDouble, log1pDouble, sqrtDouble, fabsDouble :: Double -> Double
sinDouble, cosDouble, tanDouble  :: Double -> Double
asinDouble, acosDouble, atanDouble  :: Double -> Double
sinhDouble, coshDouble, tanhDouble  :: Double -> Double
asinhDouble, acoshDouble, atanhDouble  :: Double -> Double
expDouble    (D# x) = D# (expDouble# x)
expm1Double  (D# x) = D# (expm1Double# x)
logDouble    (D# x) = D# (logDouble# x)
log1pDouble  (D# x) = D# (log1pDouble# x)
sqrtDouble   (D# x) = D# (sqrtDouble# x)
fabsDouble   (D# x) = D# (fabsDouble# x)
sinDouble    (D# x) = D# (sinDouble# x)
cosDouble    (D# x) = D# (cosDouble# x)
tanDouble    (D# x) = D# (tanDouble# x)
asinDouble   (D# x) = D# (asinDouble# x)
acosDouble   (D# x) = D# (acosDouble# x)
atanDouble   (D# x) = D# (atanDouble# x)
sinhDouble   (D# x) = D# (sinhDouble# x)
coshDouble   (D# x) = D# (coshDouble# x)
tanhDouble   (D# x) = D# (tanhDouble# x)
asinhDouble  (D# x) = D# (asinhDouble# x)
acoshDouble  (D# x) = D# (acoshDouble# x)
atanhDouble  (D# x) = D# (atanhDouble# x)

powerDouble :: Double -> Double -> Double
powerDouble  (D# x) (D# y) = D# (x **## y)

foreign import ccall unsafe "isFloatNaN" isFloatNaN :: Float -> Int
foreign import ccall unsafe "isFloatInfinite" isFloatInfinite :: Float -> Int
foreign import ccall unsafe "isFloatDenormalized" isFloatDenormalized :: Float -> Int
foreign import ccall unsafe "isFloatNegativeZero" isFloatNegativeZero :: Float -> Int
foreign import ccall unsafe "isFloatFinite" isFloatFinite :: Float -> Int

foreign import ccall unsafe "isDoubleNaN" isDoubleNaN :: Double -> Int
foreign import ccall unsafe "isDoubleInfinite" isDoubleInfinite :: Double -> Int
foreign import ccall unsafe "isDoubleDenormalized" isDoubleDenormalized :: Double -> Int
foreign import ccall unsafe "isDoubleNegativeZero" isDoubleNegativeZero :: Double -> Int
foreign import ccall unsafe "isDoubleFinite" isDoubleFinite :: Double -> Int

------------------------------------------------------------------------
-- Coercion rules
------------------------------------------------------------------------

word2Double :: Word -> Double
word2Double (W# w) = D# (word2Double# w)

word2Float :: Word -> Float
word2Float (W# w) = F# (word2Float# w)

{-# RULES
"realToFrac/Float->Float"   realToFrac   = id :: Float -> Float
"realToFrac/Float->Double"  realToFrac   = float2Double
"realToFrac/Double->Float"  realToFrac   = double2Float
"realToFrac/Double->Double" realToFrac   = id :: Double -> Double
"realToFrac/Int->Double"    realToFrac   = int2Double   -- See Note [realToFrac int-to-float]
"realToFrac/Int->Float"     realToFrac   = int2Float    --      ..ditto
    #-}

{-
Note [realToFrac int-to-float]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don found that the RULES for realToFrac/Int->Double and similarly
Float made a huge difference to some stream-fusion programs.  Here's
an example

      import Data.Array.Vector

      n = 40000000

      main = do
            let c = replicateU n (2::Double)
                a = mapU realToFrac (enumFromToU 0 (n-1) ) :: UArr Double
            print (sumU (zipWithU (*) c a))

Without the RULE we get this loop body:

      case $wtoRational sc_sY4 of ww_aM7 { (# ww1_aM9, ww2_aMa #) ->
      case $wfromRat ww1_aM9 ww2_aMa of tpl_X1P { D# ipv_sW3 ->
      Main.$s$wfold
        (+# sc_sY4 1)
        (+# wild_X1i 1)
        (+## sc2_sY6 (*## 2.0 ipv_sW3))

And with the rule:

     Main.$s$wfold
        (+# sc_sXT 1)
        (+# wild_X1h 1)
        (+## sc2_sXV (*## 2.0 (int2Double# sc_sXT)))

The running time of the program goes from 120 seconds to 0.198 seconds
with the native backend, and 0.143 seconds with the C backend.

A few more details in #2251, and the patch message
"Add RULES for realToFrac from Int".
-}

-- Utils

showSignedFloat :: (RealFloat a)
  => (a -> ShowS)       -- ^ a function that can show unsigned values
  -> Int                -- ^ the precedence of the enclosing context
  -> a                  -- ^ the value to show
  -> ShowS
showSignedFloat showPos p x
   | x < 0 || isNegativeZero x
       = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x

{-
We need to prevent over/underflow of the exponent in encodeFloat when
called from scaleFloat, hence we clamp the scaling parameter.
We must have a large enough range to cover the maximum difference of
exponents returned by decodeFloat.
-}

-- | Used to prevent exponent over/underflow when encoding floating point numbers.
--   This is also the same as
--
--   > \(x,y) -> max (-x) (min x y)
--
-- ==== __Example__
-- >>> clamp (-10) 5
-- 10
clamp :: Int -> Int -> Int
clamp bd k = max (-bd) (min bd k)


{-
Note [Casting from integral to floating point types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To implement something like `reinterpret_cast` from C++ to go from a
floating-point type to an integral type one might naively think that the
following should work:

      cast :: Float -> Word32
      cast (F# f#) = W32# (unsafeCoerce# f#)

Unfortunately that is not the case, because all the `unsafeCoerce#` does is tell
the compiler that the types have changed. When one does the above cast and
tries to operate on the resulting `Word32` the code generator will generate code
that performs an integer/word operation on a floating-point register, which
results in a compile error.

The correct way of implementing `reinterpret_cast` to implement a primop, but
that requires a unique implementation for all supported architectures. The next
best solution is to write the value from the source register to memory and then
read it from memory into the destination register and the best way to do that
is using CMM.
-}

-- | @'castWord32ToFloat' w@ does a bit-for-bit copy from an integral value
-- to a floating-point value.
--
-- @since 4.10.0.0

{-# INLINE castWord32ToFloat #-}
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# w#) = F# (stgWord32ToFloat w#)

foreign import prim "stg_word32ToFloatzh"
    stgWord32ToFloat :: Word32# -> Float#


-- | @'castFloatToWord32' f@ does a bit-for-bit copy from a floating-point value
-- to an integral value.
--
-- @since 4.10.0.0

{-# INLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# f#) = W32# (stgFloatToWord32 f#)

foreign import prim "stg_floatToWord32zh"
    stgFloatToWord32 :: Float# -> Word32#



-- | @'castWord64ToDouble' w@ does a bit-for-bit copy from an integral value
-- to a floating-point value.
--
-- @since 4.10.0.0

{-# INLINE castWord64ToDouble #-}
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# w) = D# (stgWord64ToDouble w)

foreign import prim "stg_word64ToDoublezh"
    stgWord64ToDouble :: Word64# -> Double#


-- | @'castFloatToWord64' f@ does a bit-for-bit copy from a floating-point value
-- to an integral value.
--
-- @since 4.10.0.0

{-# INLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# d#) = W64# (stgDoubleToWord64 d#)

foreign import prim "stg_doubleToWord64zh"
    stgDoubleToWord64 :: Double# -> Word64#



-- See Note [Optimising conversions between numeric types]
-- in GHC.Num.Integer
{-# RULES

"Int# -> Integer -> Float#"
  forall x. integerToFloat# (IS x) = int2Float# x

"Int# -> Integer -> Double#"
  forall x. integerToDouble# (IS x) = int2Double# x

"Word# -> Integer -> Float#"
  forall x. integerToFloat# (integerFromWord# x) = word2Float# x

"Word# -> Integer -> Double#"
  forall x. integerToDouble# (integerFromWord# x) = word2Double# x

"Word# -> Natural -> Float#"
  forall x. naturalToFloat# (NS x) = word2Float# x

"Word# -> Natural -> Double#"
  forall x. naturalToDouble# (NS x) = word2Double# x #-}
