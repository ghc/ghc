{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface,
    NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Float.RealFracMethods
-- Copyright   :  (c) Daniel Fischer 2010
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Methods for the RealFrac instances for 'Float' and 'Double',
-- with specialised versions for 'Int'.
--
-- Moved to their own module to not bloat GHC.Float further.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Float.RealFracMethods
    ( -- * Double methods
      -- ** Integer results
      properFractionDoubleInteger
    , truncateDoubleInteger
    , floorDoubleInteger
    , ceilingDoubleInteger
    , roundDoubleInteger
      -- ** Int results
    , properFractionDoubleInt
    , floorDoubleInt
    , ceilingDoubleInt
    , roundDoubleInt
      -- * Double/Int conversions, wrapped primops
    , double2Int
    , int2Double
      -- * Float methods
      -- ** Integer results
    , properFractionFloatInteger
    , truncateFloatInteger
    , floorFloatInteger
    , ceilingFloatInteger
    , roundFloatInteger
      -- ** Int results
    , properFractionFloatInt
    , floorFloatInt
    , ceilingFloatInt
    , roundFloatInt
      -- * Float/Int conversions, wrapped primops
    , float2Int
    , int2Float
    ) where

import GHC.Integer

import GHC.Base
import GHC.Num ()

#if WORD_SIZE_IN_BITS < 64

import GHC.IntWord64

#define TO64 integerToInt64
#define FROM64 int64ToInteger
#define MINUS64 minusInt64#
#define NEGATE64 negateInt64#

#else

#define TO64 integerToInt
#define FROM64 smallInteger
#define MINUS64 ( -# )
#define NEGATE64 negateInt#

uncheckedIShiftRA64# :: Int# -> Int# -> Int#
uncheckedIShiftRA64# = uncheckedIShiftRA#

uncheckedIShiftL64# :: Int# -> Int# -> Int#
uncheckedIShiftL64# = uncheckedIShiftL#

#endif

default ()

------------------------------------------------------------------------------
--                              Float Methods                               --
------------------------------------------------------------------------------

-- Special Functions for Int, nice, easy and fast.
-- They should be small enough to be inlined automatically.

-- We have to test for ±0.0 to avoid returning -0.0 in the second
-- component of the pair. Unfortunately the branching costs a lot
-- of performance.
properFractionFloatInt :: Float -> (Int, Float)
properFractionFloatInt (F# x) =
    if isTrue# (x `eqFloat#` 0.0#)
        then (I# 0#, F# 0.0#)
        else case float2Int# x of
                n -> (I# n, F# (x `minusFloat#` int2Float# n))

-- truncateFloatInt = float2Int

floorFloatInt :: Float -> Int
floorFloatInt (F# x) =
    case float2Int# x of
      n | isTrue# (x `ltFloat#` int2Float# n) -> I# (n -# 1#)
        | otherwise                           -> I# n

ceilingFloatInt :: Float -> Int
ceilingFloatInt (F# x) =
    case float2Int# x of
      n | isTrue# (int2Float# n `ltFloat#` x) -> I# (n +# 1#)
        | otherwise                           -> I# n

roundFloatInt :: Float -> Int
roundFloatInt x = float2Int (c_rintFloat x)

-- Functions with Integer results

-- With the new code generator in GHC 7, the explicit bit-fiddling is
-- slower than the old code for values of small modulus, but when the
-- 'Int' range is left, the bit-fiddling quickly wins big, so we use that.
-- If the methods are called on smallish values, hopefully people go
-- through Int and not larger types.

-- Note: For negative exponents, we must check the validity of the shift
-- distance for the right shifts of the mantissa.

{-# INLINE properFractionFloatInteger #-}
properFractionFloatInteger :: Float -> (Integer, Float)
properFractionFloatInteger v@(F# x) =
    case decodeFloat_Int# x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 23#) -> (0, v)
              | isTrue# (m <#  0#) ->
                case negateInt# (negateInt# m `uncheckedIShiftRA#` s) of
                  k -> (smallInteger k,
                            case m -# (k `uncheckedIShiftL#` s) of
                              r -> F# (encodeFloatInteger (smallInteger r) e))
              | otherwise           ->
                case m `uncheckedIShiftRL#` s of
                  k -> (smallInteger k,
                            case m -# (k `uncheckedIShiftL#` s) of
                              r -> F# (encodeFloatInteger (smallInteger r) e))
        | otherwise -> (shiftLInteger (smallInteger m) e, F# 0.0#)

{-# INLINE truncateFloatInteger #-}
truncateFloatInteger :: Float -> Integer
truncateFloatInteger x =
    case properFractionFloatInteger x of
      (n, _) -> n

-- floor is easier for negative numbers than truncate, so this gets its
-- own implementation, it's a little faster.
{-# INLINE floorFloatInteger #-}
floorFloatInteger :: Float -> Integer
floorFloatInteger (F# x) =
    case decodeFloat_Int# x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 23#) -> if isTrue# (m <# 0#) then (-1) else 0
              | otherwise          -> smallInteger (m `uncheckedIShiftRA#` s)
        | otherwise -> shiftLInteger (smallInteger m) e

-- ceiling x = -floor (-x)
-- If giving this its own implementation is faster at all,
-- it's only marginally so, hence we keep it short.
{-# INLINE ceilingFloatInteger #-}
ceilingFloatInteger :: Float -> Integer
ceilingFloatInteger (F# x) =
    negateInteger (floorFloatInteger (F# (negateFloat# x)))

{-# INLINE roundFloatInteger #-}
roundFloatInteger :: Float -> Integer
roundFloatInteger x = float2Integer (c_rintFloat x)

------------------------------------------------------------------------------
--                              Double Methods                              --
------------------------------------------------------------------------------

-- Special Functions for Int, nice, easy and fast.
-- They should be small enough to be inlined automatically.

-- We have to test for ±0.0 to avoid returning -0.0 in the second
-- component of the pair. Unfortunately the branching costs a lot
-- of performance.
properFractionDoubleInt :: Double -> (Int, Double)
properFractionDoubleInt (D# x) =
    if isTrue# (x ==## 0.0##)
        then (I# 0#, D# 0.0##)
        else case double2Int# x of
                n -> (I# n, D# (x -## int2Double# n))

-- truncateDoubleInt = double2Int

floorDoubleInt :: Double -> Int
floorDoubleInt (D# x) =
    case double2Int# x of
      n | isTrue# (x <## int2Double# n) -> I# (n -# 1#)
        | otherwise                     -> I# n

ceilingDoubleInt :: Double -> Int
ceilingDoubleInt (D# x) =
    case double2Int# x of
      n | isTrue# (int2Double# n <## x) -> I# (n +# 1#)
        | otherwise                     -> I# n

roundDoubleInt :: Double -> Int
roundDoubleInt x = double2Int (c_rintDouble x)

-- Functions with Integer results

-- The new Code generator isn't quite as good for the old 'Double' code
-- as for the 'Float' code, so for 'Double' the bit-fiddling also wins
-- when the values have small modulus.

-- When the exponent is negative, all mantissae have less than 64 bits
-- and the right shifting of sized types is much faster than that of
-- 'Integer's, especially when we can

-- Note: For negative exponents, we must check the validity of the shift
-- distance for the right shifts of the mantissa.

{-# INLINE properFractionDoubleInteger #-}
properFractionDoubleInteger :: Double -> (Integer, Double)
properFractionDoubleInteger v@(D# x) =
    case decodeDoubleInteger x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 52#) -> (0, v)
              | m < 0                 ->
                case TO64 (negateInteger m) of
                  n ->
                    case n `uncheckedIShiftRA64#` s of
                      k ->
                        (FROM64 (NEGATE64 k),
                          case MINUS64 n (k `uncheckedIShiftL64#` s) of
                            r ->
                              D# (encodeDoubleInteger (FROM64 (NEGATE64 r)) e))
              | otherwise           ->
                case TO64 m of
                  n ->
                    case n `uncheckedIShiftRA64#` s of
                      k -> (FROM64 k,
                            case MINUS64 n (k `uncheckedIShiftL64#` s) of
                              r -> D# (encodeDoubleInteger (FROM64 r) e))
        | otherwise -> (shiftLInteger m e, D# 0.0##)

{-# INLINE truncateDoubleInteger #-}
truncateDoubleInteger :: Double -> Integer
truncateDoubleInteger x =
    case properFractionDoubleInteger x of
      (n, _) -> n

-- floor is easier for negative numbers than truncate, so this gets its
-- own implementation, it's a little faster.
{-# INLINE floorDoubleInteger #-}
floorDoubleInteger :: Double -> Integer
floorDoubleInteger (D# x) =
    case decodeDoubleInteger x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 52#) -> if m < 0 then (-1) else 0
              | otherwise          ->
                case TO64 m of
                  n -> FROM64 (n `uncheckedIShiftRA64#` s)
        | otherwise -> shiftLInteger m e

{-# INLINE ceilingDoubleInteger #-}
ceilingDoubleInteger :: Double -> Integer
ceilingDoubleInteger (D# x) =
    negateInteger (floorDoubleInteger (D# (negateDouble# x)))

{-# INLINE roundDoubleInteger #-}
roundDoubleInteger :: Double -> Integer
roundDoubleInteger x = double2Integer (c_rintDouble x)

-- Wrappers around double2Int#, int2Double#, float2Int# and int2Float#,
-- we need them here, so we move them from GHC.Float and re-export them
-- explicitly from there.

double2Int :: Double -> Int
double2Int (D# x) = I# (double2Int# x)

int2Double :: Int -> Double
int2Double (I# i) = D# (int2Double# i)

float2Int :: Float -> Int
float2Int (F# x) = I# (float2Int# x)

int2Float :: Int -> Float
int2Float (I# i) = F# (int2Float# i)

-- Quicker conversions from 'Double' and 'Float' to 'Integer',
-- assuming the floating point value is integral.
--
-- Note: Since the value is integral, the exponent can't be less than
-- (-TYP_MANT_DIG), so we need not check the validity of the shift
-- distance for the right shfts here.

{-# INLINE double2Integer #-}
double2Integer :: Double -> Integer
double2Integer (D# x) =
    case decodeDoubleInteger x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case TO64 m of
            n -> FROM64 (n `uncheckedIShiftRA64#` negateInt# e)
        | otherwise -> shiftLInteger m e

{-# INLINE float2Integer #-}
float2Integer :: Float -> Integer
float2Integer (F# x) =
    case decodeFloat_Int# x of
      (# m, e #)
        | isTrue# (e <# 0#) -> smallInteger (m `uncheckedIShiftRA#` negateInt# e)
        | otherwise         -> shiftLInteger (smallInteger m) e

-- Foreign imports, the rounding is done faster in C when the value
-- isn't integral, so we call out for rounding. For values of large
-- modulus, calling out to C is slower than staying in Haskell, but
-- presumably 'round' is mostly called for values with smaller modulus,
-- when calling out to C is a major win.
-- For all other functions, calling out to C gives at most a marginal
-- speedup for values of small modulus and is much slower than staying
-- in Haskell for values of large modulus, so those are done in Haskell.

foreign import ccall unsafe "rintDouble"
    c_rintDouble :: Double -> Double

foreign import ccall unsafe "rintFloat"
    c_rintFloat :: Float -> Float

