{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns, PatternGuards #-}
module Data.Bits.Compat (
  module Base
, bitDefault
, testBitDefault
, popCountDefault
#if MIN_VERSION_base(4,7,0)
, toIntegralSized
#endif
) where

import Data.Bits as Base

#if !(MIN_VERSION_base(4,8,0))
import Prelude
#endif

#if !(MIN_VERSION_base(4,6,0))
-- | Default implementation for 'bit'.
--
-- Note that: @bitDefault i = 1 `shiftL` i@
--
-- /Since: 4.6.0.0/
bitDefault :: (Bits a, Num a) => Int -> a
bitDefault = \i -> 1 `shiftL` i
{-# INLINE bitDefault #-}

-- | Default implementation for 'testBit'.
--
-- Note that: @testBitDefault x i = (x .&. bit i) /= 0@
--
-- /Since: 4.6.0.0/
testBitDefault ::  (Bits a, Num a) => a -> Int -> Bool
testBitDefault = \x i -> (x .&. bit i) /= 0
{-# INLINE testBitDefault #-}

-- | Default implementation for 'popCount'.
--
-- This implementation is intentionally naive. Instances are expected to provide
-- an optimized implementation for their size.
--
-- /Since: 4.6.0.0/
popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
 where
   go !c 0 = c
   go c w = go (c+1) (w .&. (w - 1)) -- clear the least significant
{-# INLINABLE popCountDefault #-}
#endif

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
-- | Attempt to convert an 'Integral' type @a@ to an 'Integral' type @b@ using
-- the size of the types as measured by 'Bits' methods.
--
-- A simpler version of this function is:
--
-- > toIntegral :: (Integral a, Integral b) => a -> Maybe b
-- > toIntegral x
-- >   | toInteger x == y = Just (fromInteger y)
-- >   | otherwise        = Nothing
-- >   where
-- >     y = toInteger x
--
-- This version requires going through 'Integer', which can be inefficient.
-- However, @toIntegralSized@ is optimized to allow GHC to statically determine
-- the relative type sizes (as measured by 'bitSizeMaybe' and 'isSigned') and
-- avoid going through 'Integer' for many types. (The implementation uses
-- 'fromIntegral', which is itself optimized with rules for @base@ types but may
-- go through 'Integer' for some type pairs.)
--
-- /Since: 4.8.0.0/

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x                 -- See Note [toIntegralSized optimization]
  | maybe True (<= x) yMinBound
  , maybe True (x <=) yMaxBound = Just y
  | otherwise                   = Nothing
  where
    y = fromIntegral x

    xWidth = bitSizeMaybe x
    yWidth = bitSizeMaybe y

    yMinBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y) = Just 0
      | isSigned x, isSigned y
      , Just yW <- yWidth = Just (negate $ bit (yW-1)) -- Assumes sub-type
      | otherwise = Nothing

    yMaxBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y)
      , Just xW <- xWidth, Just yW <- yWidth
      , xW <= yW+1 = Nothing -- Max bound beyond a's domain
      | Just yW <- yWidth = if isSigned y
                            then Just (bit (yW-1)-1)
                            else Just (bit yW-1)
      | otherwise = Nothing
{-# INLINEABLE toIntegralSized #-}

-- | 'True' if the size of @a@ is @<=@ the size of @b@, where size is measured
-- by 'bitSizeMaybe' and 'isSigned'.
isBitSubType :: (Bits a, Bits b) => a -> b -> Bool
isBitSubType x y
  -- Reflexive
  | xWidth == yWidth, xSigned == ySigned = True

  -- Every integer is a subset of 'Integer'
  | ySigned, Nothing == yWidth                  = True
  | not xSigned, not ySigned, Nothing == yWidth = True

  -- Sub-type relations between fixed-with types
  | xSigned == ySigned,   Just xW <- xWidth, Just yW <- yWidth = xW <= yW
  | not xSigned, ySigned, Just xW <- xWidth, Just yW <- yWidth = xW <  yW

  | otherwise = False
  where
    xWidth  = bitSizeMaybe x
    xSigned = isSigned     x

    yWidth  = bitSizeMaybe y
    ySigned = isSigned     y
{-# INLINE isBitSubType #-}
#endif
