{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Int
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The sized integral datatypes, 'Int8', 'Int16', 'Int32', and 'Int64'.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Int (
        Int(..), Int8(..), Int16(..), Int32(..), Int64(..),
        uncheckedIShiftL64#, uncheckedIShiftRA64#,
        shiftRLInt8#, shiftRLInt16#, shiftRLInt32#,

        -- * Equality operators
        -- | See GHC.Classes#matching_overloaded_methods_in_rules
        eqInt, neInt, gtInt, geInt, ltInt, leInt,
        eqInt8, neInt8, gtInt8, geInt8, ltInt8, leInt8,
        eqInt16, neInt16, gtInt16, geInt16, ltInt16, leInt16,
        eqInt32, neInt32, gtInt32, geInt32, ltInt32, leInt32,
        eqInt64, neInt64, gtInt64, geInt64, ltInt64, leInt64

    ) where

import Data.Bits
import Data.Maybe

import GHC.Prim
import GHC.Base

import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Show

------------------------------------------------------------------------
-- type Int8
------------------------------------------------------------------------

-- Int8 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data {-# CTYPE "HsInt8" #-} Int8 = I8# Int8#
-- ^ 8-bit signed integer type

-- See GHC.Classes#matching_overloaded_methods_in_rules
-- | @since 2.01
instance Eq Int8 where
    (==) = eqInt8
    (/=) = neInt8

eqInt8, neInt8 :: Int8 -> Int8 -> Bool
eqInt8 (I8# x) (I8# y) = isTrue# ((int8ToInt# x) ==# (int8ToInt# y))
neInt8 (I8# x) (I8# y) = isTrue# ((int8ToInt# x) /=# (int8ToInt# y))
{-# INLINE [1] eqInt8 #-}
{-# INLINE [1] neInt8 #-}

-- | @since 2.01
instance Ord Int8 where
    (<)  = ltInt8
    (<=) = leInt8
    (>=) = geInt8
    (>)  = gtInt8

{-# INLINE [1] gtInt8 #-}
{-# INLINE [1] geInt8 #-}
{-# INLINE [1] ltInt8 #-}
{-# INLINE [1] leInt8 #-}
gtInt8, geInt8, ltInt8, leInt8 :: Int8 -> Int8 -> Bool
(I8# x) `gtInt8` (I8# y) = isTrue# (x `gtInt8#` y)
(I8# x) `geInt8` (I8# y) = isTrue# (x `geInt8#` y)
(I8# x) `ltInt8` (I8# y) = isTrue# (x `ltInt8#` y)
(I8# x) `leInt8` (I8# y) = isTrue# (x `leInt8#` y)

-- | @since 2.01
instance Show Int8 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

-- | @since 2.01
instance Num Int8 where
    (I8# x#) + (I8# y#)    = I8# (x# `plusInt8#` y#)
    (I8# x#) - (I8# y#)    = I8# (x# `subInt8#` y#)
    (I8# x#) * (I8# y#)    = I8# (x# `timesInt8#` y#)
    negate (I8# x#)        = I8# (negateInt8# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I8# (intToInt8# (integerToInt# i))

-- | @since 2.01
instance Real Int8 where
    toRational x = toInteger x % 1

-- | @since 2.01
instance Enum Int8 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int8"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int8"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int8) && i <= fromIntegral (maxBound::Int8)
                        = I8# (intToInt8# i#)
        | otherwise     = toEnumError "Int8" i (minBound::Int8, maxBound::Int8)
    fromEnum (I8# x#)   = I# (int8ToInt# x#)
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFrom #-}
    enumFrom            = boundedEnumFrom
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThen #-}
    enumFromThen        = boundedEnumFromThen

-- | @since 2.01
instance Integral Int8 where
    quot    x@(I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I8# (x# `quotInt8#` y#)
    rem     (I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
          -- The quotRem CPU instruction might fail for 'minBound
          -- `quotRem` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `rem` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I8# (x# `remInt8#` y#)
    div     x@(I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I8# (x# `divInt8#` y#)
    mod       (I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
          -- The divMod CPU instruction might fail for 'minBound
          -- `divMod` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `mod` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I8# (x# `modInt8#` y#)
    quotRem x@(I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case x# `quotRemInt8#` y# of
                                       (# q, r #) -> (I8# q, I8# r)
    divMod  x@(I8# x#) y@(I8# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case  x# `divModInt8#` y# of
                                       (# d, m #) -> (I8# d, I8# m)
    toInteger (I8# x#)               = IS (int8ToInt# x#)

-- | @since 2.01
instance Bounded Int8 where
    minBound = -0x80
    maxBound =  0x7F

-- | @since 2.01
instance Ix Int8 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

-- | @since 2.01
instance Read Int8 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

-- | @since 2.01
instance Bits Int8 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    {-# INLINE popCount #-}

    (I8# x#) .&.   (I8# y#)   = I8# (intToInt8# ((int8ToInt# x#) `andI#` (int8ToInt# y#)))
    (I8# x#) .|.   (I8# y#)   = I8# (intToInt8# ((int8ToInt# x#) `orI#`  (int8ToInt# y#)))
    (I8# x#) `xor` (I8# y#)   = I8# (intToInt8# ((int8ToInt# x#) `xorI#` (int8ToInt# y#)))
    complement (I8# x#)       = I8# (intToInt8# (notI# (int8ToInt# x#)))
    (I8# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#) = I8# (intToInt8# ((int8ToInt# x#) `iShiftL#` i#))
        | otherwise           = I8# (intToInt8# ((int8ToInt# x#) `iShiftRA#` negateInt# i#))
    (I8# x#) `shiftL`       (I# i#)
        | isTrue# (i# >=# 0#) = I8# (intToInt8# ((int8ToInt# x#) `iShiftL#` i#))
        | otherwise           = overflowError
    (I8# x#) `unsafeShiftL` (I# i#) = I8# (intToInt8# ((int8ToInt# x#) `uncheckedIShiftL#` i#))
    (I8# x#) `shiftR`       (I# i#)
        | isTrue# (i# >=# 0#) = I8# (intToInt8# ((int8ToInt# x#) `iShiftRA#` i#))
        | otherwise           = overflowError
    (I8# x#) `unsafeShiftR` (I# i#) = I8# (intToInt8# ((int8ToInt# x#) `uncheckedIShiftRA#` i#))
    (I8# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = I8# x#
        | otherwise
        = I8# (intToInt8# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                       (x'# `uncheckedShiftRL#` (8# -# i'#)))))
        where
        !x'# = narrow8Word# (int2Word# (int8ToInt# x#))
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = True
    popCount (I8# x#)         = I# (word2Int# (popCnt8# (int2Word# (int8ToInt# x#))))
    bit i                     = bitDefault i
    testBit a i               = testBitDefault a i

-- | @since 4.6.0.0
instance FiniteBits Int8 where
    {-# INLINE countLeadingZeros #-}
    {-# INLINE countTrailingZeros #-}
    finiteBitSize _ = 8
    countLeadingZeros  (I8# x#) = I# (word2Int# (clz8# (int2Word# (int8ToInt# x#))))
    countTrailingZeros (I8# x#) = I# (word2Int# (ctz8# (int2Word# (int8ToInt# x#))))

{-# RULES
"properFraction/Float->(Int8,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int8) n, y :: Float) }
"truncate/Float->Int8"
    truncate = (fromIntegral :: Int -> Int8) . (truncate :: Float -> Int)
"floor/Float->Int8"
    floor    = (fromIntegral :: Int -> Int8) . (floor :: Float -> Int)
"ceiling/Float->Int8"
    ceiling  = (fromIntegral :: Int -> Int8) . (ceiling :: Float -> Int)
"round/Float->Int8"
    round    = (fromIntegral :: Int -> Int8) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Int8,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int8) n, y :: Double) }
"truncate/Double->Int8"
    truncate = (fromIntegral :: Int -> Int8) . (truncate :: Double -> Int)
"floor/Double->Int8"
    floor    = (fromIntegral :: Int -> Int8) . (floor :: Double -> Int)
"ceiling/Double->Int8"
    ceiling  = (fromIntegral :: Int -> Int8) . (ceiling :: Double -> Int)
"round/Double->Int8"
    round    = (fromIntegral :: Int -> Int8) . (round  :: Double -> Int)
  #-}

------------------------------------------------------------------------
-- type Int16
------------------------------------------------------------------------

-- Int16 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data {-# CTYPE "HsInt16" #-} Int16 = I16# Int16#
-- ^ 16-bit signed integer type

-- See GHC.Classes#matching_overloaded_methods_in_rules
-- | @since 2.01
instance Eq Int16 where
    (==) = eqInt16
    (/=) = neInt16

eqInt16, neInt16 :: Int16 -> Int16 -> Bool
eqInt16 (I16# x) (I16# y) = isTrue# ((int16ToInt# x) ==# (int16ToInt# y))
neInt16 (I16# x) (I16# y) = isTrue# ((int16ToInt# x) /=# (int16ToInt# y))
{-# INLINE [1] eqInt16 #-}
{-# INLINE [1] neInt16 #-}

-- | @since 2.01
instance Ord Int16 where
    (<)  = ltInt16
    (<=) = leInt16
    (>=) = geInt16
    (>)  = gtInt16

{-# INLINE [1] gtInt16 #-}
{-# INLINE [1] geInt16 #-}
{-# INLINE [1] ltInt16 #-}
{-# INLINE [1] leInt16 #-}
gtInt16, geInt16, ltInt16, leInt16 :: Int16 -> Int16 -> Bool
(I16# x) `gtInt16` (I16# y) = isTrue# (x `gtInt16#` y)
(I16# x) `geInt16` (I16# y) = isTrue# (x `geInt16#` y)
(I16# x) `ltInt16` (I16# y) = isTrue# (x `ltInt16#` y)
(I16# x) `leInt16` (I16# y) = isTrue# (x `leInt16#` y)

-- | @since 2.01
instance Show Int16 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

-- | @since 2.01
instance Num Int16 where
    (I16# x#) + (I16# y#)  = I16# (x# `plusInt16#` y#)
    (I16# x#) - (I16# y#)  = I16# (x# `subInt16#` y#)
    (I16# x#) * (I16# y#)  = I16# (x# `timesInt16#` y#)
    negate (I16# x#)       = I16# (negateInt16# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I16# (intToInt16# (integerToInt# i))

-- | @since 2.01
instance Real Int16 where
    toRational x = toInteger x % 1

-- | @since 2.01
instance Enum Int16 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int16"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int16"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int16) && i <= fromIntegral (maxBound::Int16)
                        = I16# (intToInt16# i#)
        | otherwise     = toEnumError "Int16" i (minBound::Int16, maxBound::Int16)
    fromEnum (I16# x#)  = I# (int16ToInt# x#)
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFrom #-}
    enumFrom            = boundedEnumFrom
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThen #-}
    enumFromThen        = boundedEnumFromThen

-- | @since 2.01
instance Integral Int16 where
    quot    x@(I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I16# (x# `quotInt16#` y#)
    rem       (I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
          -- The quotRem CPU instruction might fail for 'minBound
          -- `quotRem` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `rem` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I16# (x# `remInt16#` y#)
    div     x@(I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I16# (x# `divInt16#` y#)
    mod       (I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
          -- The divMod CPU instruction might fail for 'minBound
          -- `divMod` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `mod` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I16# (x# `modInt16#` y#)
    quotRem x@(I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case x# `quotRemInt16#` y# of
                                       (# q, r #) -> (I16# q, I16# r)
    divMod  x@(I16# x#) y@(I16# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case x# `divModInt16#` y# of
                                       (# d, m #) -> (I16# d, I16# m)
    toInteger (I16# x#)              = IS (int16ToInt# x#)

-- | @since 2.01
instance Bounded Int16 where
    minBound = -0x8000
    maxBound =  0x7FFF

-- | @since 2.01
instance Ix Int16 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

-- | @since 2.01
instance Read Int16 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

-- | @since 2.01
instance Bits Int16 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    {-# INLINE popCount #-}

    (I16# x#) .&.   (I16# y#)  = I16# (intToInt16# ((int16ToInt# x#) `andI#` (int16ToInt# y#)))
    (I16# x#) .|.   (I16# y#)  = I16# (intToInt16# ((int16ToInt# x#) `orI#`  (int16ToInt# y#)))
    (I16# x#) `xor` (I16# y#)  = I16# (intToInt16# ((int16ToInt# x#) `xorI#` (int16ToInt# y#)))
    complement (I16# x#)       = I16# (intToInt16# (notI# (int16ToInt# x#)))
    (I16# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = I16# (intToInt16# ((int16ToInt# x#) `iShiftL#` i#))
        | otherwise            = I16# (intToInt16# ((int16ToInt# x#) `iShiftRA#` negateInt# i#))
    (I16# x#) `shiftL`       (I# i#)
        | isTrue# (i# >=# 0#)  = I16# (intToInt16# ((int16ToInt# x#) `iShiftL#` i#))
        | otherwise            = overflowError
    (I16# x#) `unsafeShiftL` (I# i#) = I16# (intToInt16# ((int16ToInt# x#) `uncheckedIShiftL#` i#))
    (I16# x#) `shiftR`       (I# i#)
        | isTrue# (i# >=# 0#)  = I16# (intToInt16# ((int16ToInt# x#) `iShiftRA#` i#))
        | otherwise            = overflowError
    (I16# x#) `unsafeShiftR` (I# i#) = I16# (intToInt16# ((int16ToInt# x#) `uncheckedIShiftRA#` i#))
    (I16# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = I16# x#
        | otherwise
        = I16# (intToInt16# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                         (x'# `uncheckedShiftRL#` (16# -# i'#)))))
        where
        !x'# = narrow16Word# (int2Word# (int16ToInt# x#))
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitSizeMaybe i             = Just (finiteBitSize i)
    bitSize i                  = finiteBitSize i
    isSigned _                 = True
    popCount (I16# x#)         = I# (word2Int# (popCnt16# (int2Word# (int16ToInt# x#))))
    bit i                      = bitDefault i
    testBit a i                = testBitDefault a i

-- | @since 4.6.0.0
instance FiniteBits Int16 where
    {-# INLINE countLeadingZeros #-}
    {-# INLINE countTrailingZeros #-}
    finiteBitSize _ = 16
    countLeadingZeros  (I16# x#) = I# (word2Int# (clz16# (int2Word# (int16ToInt# x#))))
    countTrailingZeros (I16# x#) = I# (word2Int# (ctz16# (int2Word# (int16ToInt# x#))))

{-# RULES
"properFraction/Float->(Int16,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int16) n, y :: Float) }
"truncate/Float->Int16"
    truncate = (fromIntegral :: Int -> Int16) . (truncate :: Float -> Int)
"floor/Float->Int16"
    floor    = (fromIntegral :: Int -> Int16) . (floor :: Float -> Int)
"ceiling/Float->Int16"
    ceiling  = (fromIntegral :: Int -> Int16) . (ceiling :: Float -> Int)
"round/Float->Int16"
    round    = (fromIntegral :: Int -> Int16) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Int16,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int16) n, y :: Double) }
"truncate/Double->Int16"
    truncate = (fromIntegral :: Int -> Int16) . (truncate :: Double -> Int)
"floor/Double->Int16"
    floor    = (fromIntegral :: Int -> Int16) . (floor :: Double -> Int)
"ceiling/Double->Int16"
    ceiling  = (fromIntegral :: Int -> Int16) . (ceiling :: Double -> Int)
"round/Double->Int16"
    round    = (fromIntegral :: Int -> Int16) . (round  :: Double -> Int)
  #-}

------------------------------------------------------------------------
-- type Int32
------------------------------------------------------------------------

data {-# CTYPE "HsInt32" #-} Int32 = I32# Int32#
-- ^ 32-bit signed integer type

-- See GHC.Classes#matching_overloaded_methods_in_rules
-- | @since 2.01
instance Eq Int32 where
    (==) = eqInt32
    (/=) = neInt32

eqInt32, neInt32 :: Int32 -> Int32 -> Bool
eqInt32 (I32# x) (I32# y) = isTrue# ((int32ToInt# x) ==# (int32ToInt# y))
neInt32 (I32# x) (I32# y) = isTrue# ((int32ToInt# x) /=# (int32ToInt# y))
{-# INLINE [1] eqInt32 #-}
{-# INLINE [1] neInt32 #-}

-- | @since 2.01
instance Ord Int32 where
    (<)  = ltInt32
    (<=) = leInt32
    (>=) = geInt32
    (>)  = gtInt32

{-# INLINE [1] gtInt32 #-}
{-# INLINE [1] geInt32 #-}
{-# INLINE [1] ltInt32 #-}
{-# INLINE [1] leInt32 #-}
gtInt32, geInt32, ltInt32, leInt32 :: Int32 -> Int32 -> Bool
(I32# x) `gtInt32` (I32# y) = isTrue# (x `gtInt32#` y)
(I32# x) `geInt32` (I32# y) = isTrue# (x `geInt32#` y)
(I32# x) `ltInt32` (I32# y) = isTrue# (x `ltInt32#` y)
(I32# x) `leInt32` (I32# y) = isTrue# (x `leInt32#` y)

-- | @since 2.01
instance Show Int32 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

-- | @since 2.01
instance Num Int32 where
    (I32# x#) + (I32# y#)  = I32# (x# `plusInt32#` y#)
    (I32# x#) - (I32# y#)  = I32# (x# `subInt32#` y#)
    (I32# x#) * (I32# y#)  = I32# (x# `timesInt32#` y#)
    negate (I32# x#)       = I32# (negateInt32# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I32# (intToInt32# (integerToInt# i))

-- | @since 2.01
instance Enum Int32 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int32"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int32"
#if WORD_SIZE_IN_BITS == 32
    toEnum (I# i#)      = I32# (intToInt32# i#)
#else
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int32) && i <= fromIntegral (maxBound::Int32)
                        = I32# (intToInt32# i#)
        | otherwise     = toEnumError "Int32" i (minBound::Int32, maxBound::Int32)
#endif
    fromEnum (I32# x#)  = I# (int32ToInt# x#)
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFrom #-}
    enumFrom            = boundedEnumFrom
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThen #-}
    enumFromThen        = boundedEnumFromThen

-- | @since 2.01
instance Integral Int32 where
    quot    x@(I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I32# (x# `quotInt32#` y#)
    rem       (I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
          -- The quotRem CPU instruction might fail for 'minBound
          -- `quotRem` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `rem` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I32# (x# `remInt32#` y#)
    div     x@(I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = I32# (x# `divInt32#` y#)
    mod       (I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
          -- The divMod CPU instruction might fail for 'minBound
          -- `divMod` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `mod` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I32# (x# `modInt32#` y#)
    quotRem x@(I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case x# `quotRemInt32#` y# of
                                       (# q, r #) -> (I32# q, I32# r)
    divMod  x@(I32# x#) y@(I32# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case x# `divModInt32#` y# of
                                       (# d, m #) -> (I32# d, I32# m)
    toInteger (I32# x#)              = IS (int32ToInt# x#)

-- | @since 2.01
instance Read Int32 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

-- | @since 2.01
instance Bits Int32 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    {-# INLINE popCount #-}

    (I32# x#) .&.   (I32# y#)  = I32# (intToInt32# ((int32ToInt# x#) `andI#` (int32ToInt# y#)))
    (I32# x#) .|.   (I32# y#)  = I32# (intToInt32# ((int32ToInt# x#) `orI#`  (int32ToInt# y#)))
    (I32# x#) `xor` (I32# y#)  = I32# (intToInt32# ((int32ToInt# x#) `xorI#` (int32ToInt# y#)))
    complement (I32# x#)       = I32# (intToInt32# (notI# (int32ToInt# x#)))
    (I32# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = I32# (intToInt32# ((int32ToInt# x#) `iShiftL#` i#))
        | otherwise            = I32# (intToInt32# ((int32ToInt# x#) `iShiftRA#` negateInt# i#))
    (I32# x#) `shiftL`       (I# i#)
        | isTrue# (i# >=# 0#)  = I32# (intToInt32# ((int32ToInt# x#) `iShiftL#` i#))
        | otherwise            = overflowError
    (I32# x#) `unsafeShiftL` (I# i#) =
        I32# (intToInt32# ((int32ToInt# x#) `uncheckedIShiftL#` i#))
    (I32# x#) `shiftR`       (I# i#)
        | isTrue# (i# >=# 0#)  = I32# (intToInt32# ((int32ToInt# x#) `iShiftRA#` i#))
        | otherwise            = overflowError
    (I32# x#) `unsafeShiftR` (I# i#) = I32# (intToInt32# ((int32ToInt# x#) `uncheckedIShiftRA#` i#))
    (I32# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = I32# x#
        | otherwise
        = I32# (intToInt32# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                         (x'# `uncheckedShiftRL#` (32# -# i'#)))))
        where
        !x'# = narrow32Word# (int2Word# (int32ToInt# x#))
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    bitSizeMaybe i             = Just (finiteBitSize i)
    bitSize i                  = finiteBitSize i
    isSigned _                 = True
    popCount (I32# x#)         = I# (word2Int# (popCnt32# (int2Word# (int32ToInt# x#))))
    bit i                      = bitDefault i
    testBit a i                = testBitDefault a i

-- | @since 4.6.0.0
instance FiniteBits Int32 where
    {-# INLINE countLeadingZeros #-}
    {-# INLINE countTrailingZeros #-}
    finiteBitSize _ = 32
    countLeadingZeros  (I32# x#) = I# (word2Int# (clz32# (int2Word# (int32ToInt# x#))))
    countTrailingZeros (I32# x#) = I# (word2Int# (ctz32# (int2Word# (int32ToInt# x#))))

{-# RULES
"properFraction/Float->(Int32,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int32) n, y :: Float) }
"truncate/Float->Int32"
    truncate = (fromIntegral :: Int -> Int32) . (truncate :: Float -> Int)
"floor/Float->Int32"
    floor    = (fromIntegral :: Int -> Int32) . (floor :: Float -> Int)
"ceiling/Float->Int32"
    ceiling  = (fromIntegral :: Int -> Int32) . (ceiling :: Float -> Int)
"round/Float->Int32"
    round    = (fromIntegral :: Int -> Int32) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Int32,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int32) n, y :: Double) }
"truncate/Double->Int32"
    truncate = (fromIntegral :: Int -> Int32) . (truncate :: Double -> Int)
"floor/Double->Int32"
    floor    = (fromIntegral :: Int -> Int32) . (floor :: Double -> Int)
"ceiling/Double->Int32"
    ceiling  = (fromIntegral :: Int -> Int32) . (ceiling :: Double -> Int)
"round/Double->Int32"
    round    = (fromIntegral :: Int -> Int32) . (round  :: Double -> Int)
  #-}

-- | @since 2.01
instance Real Int32 where
    toRational x = toInteger x % 1

-- | @since 2.01
instance Bounded Int32 where
    minBound = -0x80000000
    maxBound =  0x7FFFFFFF

-- | @since 2.01
instance Ix Int32 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

------------------------------------------------------------------------
-- type Int64
------------------------------------------------------------------------

data {-# CTYPE "HsInt64" #-} Int64 = I64# Int64#
-- ^ 64-bit signed integer type

-- See GHC.Classes#matching_overloaded_methods_in_rules
-- | @since 2.01
instance Eq Int64 where
    (==) = eqInt64
    (/=) = neInt64

eqInt64, neInt64 :: Int64 -> Int64 -> Bool
eqInt64 (I64# x) (I64# y) = isTrue# (x `eqInt64#` y)
neInt64 (I64# x) (I64# y) = isTrue# (x `neInt64#` y)
{-# INLINE [1] eqInt64 #-}
{-# INLINE [1] neInt64 #-}

-- | @since 2.01
instance Ord Int64 where
    (<)  = ltInt64
    (<=) = leInt64
    (>=) = geInt64
    (>)  = gtInt64

{-# INLINE [1] gtInt64 #-}
{-# INLINE [1] geInt64 #-}
{-# INLINE [1] ltInt64 #-}
{-# INLINE [1] leInt64 #-}
gtInt64, geInt64, ltInt64, leInt64 :: Int64 -> Int64 -> Bool
(I64# x) `gtInt64` (I64# y) = isTrue# (x `gtInt64#` y)
(I64# x) `geInt64` (I64# y) = isTrue# (x `geInt64#` y)
(I64# x) `ltInt64` (I64# y) = isTrue# (x `ltInt64#` y)
(I64# x) `leInt64` (I64# y) = isTrue# (x `leInt64#` y)

-- | @since 2.01
instance Show Int64 where
    showsPrec p x = showsPrec p (toInteger x)

-- | @since 2.01
instance Num Int64 where
    (I64# x#) + (I64# y#)  = I64# (x# `plusInt64#`  y#)
    (I64# x#) - (I64# y#)  = I64# (x# `subInt64#` y#)
    (I64# x#) * (I64# y#)  = I64# (x# `timesInt64#` y#)
    negate (I64# x#)       = I64# (negateInt64# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I64# (integerToInt64# i)

-- | @since 2.01
instance Enum Int64 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int64"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int64"
    toEnum (I# i#)      = I64# (intToInt64# i#)
    fromEnum x@(I64# x#)
        | x >= fromIntegral (minBound::Int) && x <= fromIntegral (maxBound::Int)
                        = I# (int64ToInt# x#)
        | otherwise     = fromEnumError "Int64" x
#if WORD_SIZE_IN_BITS < 64
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFrom #-}
    enumFrom            = integralEnumFrom
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThen #-}
    enumFromThen        = integralEnumFromThen
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromTo #-}
    enumFromTo          = integralEnumFromTo
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo      = integralEnumFromThenTo
#else
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFrom #-}
    enumFrom            = boundedEnumFrom
    -- See Note [Stable Unfolding for list producers] in GHC.Enum
    {-# INLINE enumFromThen #-}
    enumFromThen        = boundedEnumFromThen
#endif

-- | @since 2.01
instance Integral Int64 where
    quot    x@(I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- See Note [Order of tests]
        | otherwise                  = I64# (x# `quotInt64#` y#)
    rem       (I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
          -- The quotRem CPU instruction might fail for 'minBound
          -- `quotRem` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `rem` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I64# (x# `remInt64#` y#)
    div     x@(I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- See Note [Order of tests]
        | otherwise                  = I64# (x# `divInt64#` y#)
    mod       (I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
          -- The divMod CPU instruction might fail for 'minBound
          -- `divMod` -1' if it is an instruction for exactly this
          -- width of signed integer. But, 'minBound `mod` -1' is
          -- well-defined (0). We therefore special-case it.
        | y == (-1)                  = 0
        | otherwise                  = I64# (x# `modInt64#` y#)
    quotRem x@(I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
#if WORD_SIZE_IN_BITS < 64
        -- we don't have quotRemInt64# primop yet
        | otherwise                  = (I64# (x# `quotInt64#` y#), I64# (x# `remInt64#` y#))
#else
        | otherwise                  = case quotRemInt# (int64ToInt# x#) (int64ToInt# y#) of
                                        (# q, r #) -> (I64# (intToInt64# q), I64# (intToInt64# r))
#endif
    divMod  x@(I64# x#) y@(I64# y#)
        | y == 0                     = divZeroError
          -- See Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
#if WORD_SIZE_IN_BITS < 64
        -- we don't have divModInt64# primop yet
        | otherwise                  = (I64# (x# `divInt64#` y#), I64# (x# `modInt64#` y#))
#else
        | otherwise                  = case divModInt# (int64ToInt# x#) (int64ToInt# y#) of
                                        (# q, r #) -> (I64# (intToInt64# q), I64# (intToInt64# r))
#endif
    toInteger (I64# x)               = integerFromInt64# x


divInt64#, modInt64# :: Int64# -> Int64# -> Int64#

-- Define div in terms of quot, being careful to avoid overflow (#7233)
x# `divInt64#` y#
    | isTrue# (x# `gtInt64#` zero) && isTrue# (y# `ltInt64#` zero)
        = ((x# `subInt64#` one) `quotInt64#` y#) `subInt64#` one
    | isTrue# (x# `ltInt64#` zero) && isTrue# (y# `gtInt64#` zero)
        = ((x# `plusInt64#` one)  `quotInt64#` y#) `subInt64#` one
    | otherwise
        = x# `quotInt64#` y#
    where
    !zero = intToInt64# 0#
    !one  = intToInt64# 1#

x# `modInt64#` y#
    | isTrue# (x# `gtInt64#` zero) && isTrue# (y# `ltInt64#` zero) ||
      isTrue# (x# `ltInt64#` zero) && isTrue# (y# `gtInt64#` zero)
        = if isTrue# (r# `neInt64#` zero) then r# `plusInt64#` y# else zero
    | otherwise = r#
    where
    !zero = intToInt64# 0#
    !r# = x# `remInt64#` y#

-- | @since 2.01
instance Read Int64 where
    readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s]

-- | @since 2.01
instance Bits Int64 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    {-# INLINE popCount #-}

    (I64# x#) .&.   (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `and64#` int64ToWord64# y#))
    (I64# x#) .|.   (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `or64#`  int64ToWord64# y#))
    (I64# x#) `xor` (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` int64ToWord64# y#))
    complement (I64# x#)       = I64# (word64ToInt64# (not64# (int64ToWord64# x#)))
    (I64# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = I64# (x# `shiftLInt64#` i#)
        | otherwise            = I64# (x# `shiftRAInt64#` negateInt# i#)
    (I64# x#) `shiftL` (I# i#)
        | isTrue# (i# >=# 0#)  = I64# (x# `shiftLInt64#` i#)
        | otherwise            = overflowError
    (I64# x#) `unsafeShiftL` (I# i#) = I64# (x# `uncheckedIShiftL64#` i#)
    (I64# x#) `shiftR` (I# i#)
        | isTrue# (i# >=# 0#)  = I64# (x# `shiftRAInt64#` i#)
        | otherwise            = overflowError
    (I64# x#) `unsafeShiftR` (I# i#) = I64# (x# `uncheckedIShiftRA64#` i#)
    (I64# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = I64# x#
        | otherwise
        = I64# (word64ToInt64# ((x'# `uncheckedShiftL64#` i'#) `or64#`
                                (x'# `uncheckedShiftRL64#` (64# -# i'#))))
        where
        !x'# = int64ToWord64# x#
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitSizeMaybe i             = Just (finiteBitSize i)
    bitSize i                  = finiteBitSize i
    isSigned _                 = True
    popCount (I64# x#)         =
        I# (word2Int# (popCnt64# (int64ToWord64# x#)))
    bit i                      = bitDefault i
    testBit a i                = testBitDefault a i

-- No RULES for RealFrac methods if Int is smaller than Int64, we can't
-- go through Int and whether going through Integer is faster is uncertain.

#if WORD_SIZE_IN_BITS == 64
{-# RULES
"properFraction/Float->(Int64,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int64) n, y :: Float) }
"truncate/Float->Int64"
    truncate = (fromIntegral :: Int -> Int64) . (truncate :: Float -> Int)
"floor/Float->Int64"
    floor    = (fromIntegral :: Int -> Int64) . (floor :: Float -> Int)
"ceiling/Float->Int64"
    ceiling  = (fromIntegral :: Int -> Int64) . (ceiling :: Float -> Int)
"round/Float->Int64"
    round    = (fromIntegral :: Int -> Int64) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Int64,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int64) n, y :: Double) }
"truncate/Double->Int64"
    truncate = (fromIntegral :: Int -> Int64) . (truncate :: Double -> Int)
"floor/Double->Int64"
    floor    = (fromIntegral :: Int -> Int64) . (floor :: Double -> Int)
"ceiling/Double->Int64"
    ceiling  = (fromIntegral :: Int -> Int64) . (ceiling :: Double -> Int)
"round/Double->Int64"
    round    = (fromIntegral :: Int -> Int64) . (round  :: Double -> Int)
  #-}
#endif

-- | @since 4.6.0.0
instance FiniteBits Int64 where
    {-# INLINE countLeadingZeros #-}
    {-# INLINE countTrailingZeros #-}
    finiteBitSize _ = 64
    countLeadingZeros  (I64# x#) = I# (word2Int# (clz64# (int64ToWord64# x#)))
    countTrailingZeros (I64# x#) = I# (word2Int# (ctz64# (int64ToWord64# x#)))

-- | @since 2.01
instance Real Int64 where
    toRational x = toInteger x % 1

-- | @since 2.01
instance Bounded Int64 where
    minBound = -0x8000000000000000
    maxBound =  0x7FFFFFFFFFFFFFFF

-- | @since 2.01
instance Ix Int64 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

-------------------------------------------------------------------------------

{-
Note [Order of tests]
~~~~~~~~~~~~~~~~~~~~~
(See #3065, #5161.) Suppose we had a definition like:

    quot x y
     | y == 0                     = divZeroError
     | x == minBound && y == (-1) = overflowError
     | otherwise                  = x `primQuot` y

Note in particular that the
    x == minBound
test comes before the
    y == (-1)
test.

this expands to something like:

    case y of
    0 -> divZeroError
    _ -> case x of
         -9223372036854775808 ->
             case y of
             -1 -> overflowError
             _ -> x `primQuot` y
         _ -> x `primQuot` y

Now if we have the call (x `quot` 2), and quot gets inlined, then we get:

    case 2 of
    0 -> divZeroError
    _ -> case x of
         -9223372036854775808 ->
             case 2 of
             -1 -> overflowError
             _ -> x `primQuot` 2
         _ -> x `primQuot` 2

which simplifies to:

    case x of
    -9223372036854775808 -> x `primQuot` 2
    _                    -> x `primQuot` 2

Now we have a case with two identical branches, which would be
eliminated (assuming it doesn't affect strictness, which it doesn't in
this case), leaving the desired:

    x `primQuot` 2

except in the minBound branch we know what x is, and GHC cleverly does
the division at compile time, giving:

    case x of
    -9223372036854775808 -> -4611686018427387904
    _                    -> x `primQuot` 2

So instead we use a definition like:

    quot x y
     | y == 0                     = divZeroError
     | y == (-1) && x == minBound = overflowError
     | otherwise                  = x `primQuot` y

which gives us:

    case y of
    0 -> divZeroError
    -1 ->
        case x of
        -9223372036854775808 -> overflowError
        _ -> x `primQuot` y
    _ -> x `primQuot` y

for which our call (x `quot` 2) expands to:

    case 2 of
    0 -> divZeroError
    -1 ->
        case x of
        -9223372036854775808 -> overflowError
        _ -> x `primQuot` 2
    _ -> x `primQuot` 2

which simplifies to:

    x `primQuot` 2

as required.



But we now have the same problem with a constant numerator: the call
(2 `quot` y) expands to

    case y of
    0 -> divZeroError
    -1 ->
        case 2 of
        -9223372036854775808 -> overflowError
        _ -> 2 `primQuot` y
    _ -> 2 `primQuot` y

which simplifies to:

    case y of
    0 -> divZeroError
    -1 -> 2 `primQuot` y
    _ -> 2 `primQuot` y

which simplifies to:

    case y of
    0 -> divZeroError
    -1 -> -2
    _ -> 2 `primQuot` y


However, constant denominators are more common than constant numerators,
so the
    y == (-1) && x == minBound
order gives us better code in the common case.
-}

-------------------------------------------------------------------------------

-- unchecked shift primops may be lowered into C shift operations which have
-- unspecified behaviour if the amount of bits to shift is greater or equal to the word
-- size in bits.
-- The following safe shift operations wrap unchecked primops to take this into
-- account: 0 is consistently returned when the shift amount is too big.

shiftRLInt8# :: Int8# -> Int# -> Int8#
a `shiftRLInt8#` b = uncheckedShiftRLInt8# a b `andInt8#` intToInt8# (shift_mask 8# b)

shiftRLInt16# :: Int16# -> Int# -> Int16#
a `shiftRLInt16#` b = uncheckedShiftRLInt16# a b `andInt16#` intToInt16# (shift_mask 16# b)

shiftRLInt32# :: Int32# -> Int# -> Int32#
a `shiftRLInt32#` b = uncheckedShiftRLInt32# a b `andInt32#` intToInt32# (shift_mask 32# b)



shiftLInt64# :: Int64# -> Int# -> Int64#
a `shiftLInt64#` b  = uncheckedIShiftL64# a b `andInt64#` intToInt64# (shift_mask 64# b)


shiftRAInt64# :: Int64# -> Int# -> Int64#
a `shiftRAInt64#` b | isTrue# (b >=# 64#) = intToInt64# (negateInt# (a `ltInt64#` (intToInt64# 0#)))
                    | otherwise           = a `uncheckedIShiftRA64#` b


andInt8# :: Int8# -> Int8# -> Int8#
x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)

andInt16# :: Int16# -> Int16# -> Int16#
x `andInt16#` y = word16ToInt16# (int16ToWord16# x `andWord16#` int16ToWord16# y)

andInt32# :: Int32# -> Int32# -> Int32#
x `andInt32#` y = word32ToInt32# (int32ToWord32# x `andWord32#` int32ToWord32# y)

andInt64# :: Int64# -> Int64# -> Int64#
x `andInt64#` y = word64ToInt64# (int64ToWord64# x `and64#` int64ToWord64# y)
