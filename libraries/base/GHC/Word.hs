{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Word
-- Copyright   :  (c) The University of Glasgow, 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Sized unsigned integral types: 'Word', 'Word8', 'Word16', 'Word32', and
-- 'Word64'.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

-- #hide
module GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..),
    uncheckedShiftL64#,
    uncheckedShiftRL64#,
    byteSwap16,
    byteSwap32,
    byteSwap64
    ) where

import Data.Bits
import Data.Maybe

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

-- import {-# SOURCE #-} GHC.Exception
import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Show
import GHC.Float ()     -- for RealFrac methods

------------------------------------------------------------------------
-- type Word8
------------------------------------------------------------------------

-- Word8 is represented in the same way as Word. Operations may assume
-- and must ensure that it holds only values from its logical range.

data {-# CTYPE "HsWord8" #-} Word8 = W8# Word# deriving (Eq, Ord)
-- ^ 8-bit unsigned integer type

instance Show Word8 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word8 where
    (W8# x#) + (W8# y#)    = W8# (narrow8Word# (x# `plusWord#` y#))
    (W8# x#) - (W8# y#)    = W8# (narrow8Word# (x# `minusWord#` y#))
    (W8# x#) * (W8# y#)    = W8# (narrow8Word# (x# `timesWord#` y#))
    negate (W8# x#)        = W8# (narrow8Word# (int2Word# (negateInt# (word2Int# x#))))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W8# (narrow8Word# (integerToWord i))

instance Real Word8 where
    toRational x = toInteger x % 1

instance Enum Word8 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word8"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word8"
    toEnum i@(I# i#)
        | i >= 0 && i <= fromIntegral (maxBound::Word8)
                        = W8# (int2Word# i#)
        | otherwise     = toEnumError "Word8" i (minBound::Word8, maxBound::Word8)
    fromEnum (W8# x#)   = I# (word2Int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Word8 where
    quot    (W8# x#) y@(W8# y#)
        | y /= 0                  = W8# (x# `quotWord#` y#)
        | otherwise               = divZeroError
    rem     (W8# x#) y@(W8# y#)
        | y /= 0                  = W8# (x# `remWord#` y#)
        | otherwise               = divZeroError
    div     (W8# x#) y@(W8# y#)
        | y /= 0                  = W8# (x# `quotWord#` y#)
        | otherwise               = divZeroError
    mod     (W8# x#) y@(W8# y#)
        | y /= 0                  = W8# (x# `remWord#` y#)
        | otherwise               = divZeroError
    quotRem (W8# x#) y@(W8# y#)
        | y /= 0                  = case x# `quotRemWord#` y# of
                                    (# q, r #) ->
                                        (W8# q, W8# r)
        | otherwise               = divZeroError
    divMod  (W8# x#) y@(W8# y#)
        | y /= 0                  = (W8# (x# `quotWord#` y#), W8# (x# `remWord#` y#))
        | otherwise               = divZeroError
    toInteger (W8# x#)            = smallInteger (word2Int# x#)

instance Bounded Word8 where
    minBound = 0
    maxBound = 0xFF

instance Ix Word8 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Read Word8 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word8 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W8# x#) .&.   (W8# y#)   = W8# (x# `and#` y#)
    (W8# x#) .|.   (W8# y#)   = W8# (x# `or#`  y#)
    (W8# x#) `xor` (W8# y#)   = W8# (x# `xor#` y#)
    complement (W8# x#)       = W8# (x# `xor#` mb#)
        where !(W8# mb#) = maxBound
    (W8# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#) = W8# (narrow8Word# (x# `shiftL#` i#))
        | otherwise           = W8# (x# `shiftRL#` negateInt# i#)
    (W8# x#) `shiftL`       (I# i#) = W8# (narrow8Word# (x# `shiftL#` i#))
    (W8# x#) `unsafeShiftL` (I# i#) =
        W8# (narrow8Word# (x# `uncheckedShiftL#` i#))
    (W8# x#) `shiftR`       (I# i#) = W8# (x# `shiftRL#` i#)
    (W8# x#) `unsafeShiftR` (I# i#) = W8# (x# `uncheckedShiftRL#` i#)
    (W8# x#) `rotate`       (I# i#)
        | isTrue# (i'# ==# 0#) = W8# x#
        | otherwise  = W8# (narrow8Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                          (x# `uncheckedShiftRL#` (8# -# i'#))))
        where
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W8# x#)         = I# (word2Int# (popCnt8# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word8 where
    finiteBitSize _ = 8

{-# RULES
"fromIntegral/Word8->Word8"   fromIntegral = id :: Word8 -> Word8
"fromIntegral/Word8->Integer" fromIntegral = toInteger :: Word8 -> Integer
"fromIntegral/a->Word8"       fromIntegral = \x -> case fromIntegral x of W# x# -> W8# (narrow8Word# x#)
"fromIntegral/Word8->a"       fromIntegral = \(W8# x#) -> fromIntegral (W# x#)
  #-}

{-# RULES
"properFraction/Float->(Word8,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word8) n, y :: Float) }
"truncate/Float->Word8"
    truncate = (fromIntegral :: Int -> Word8) . (truncate :: Float -> Int)
"floor/Float->Word8"
    floor    = (fromIntegral :: Int -> Word8) . (floor :: Float -> Int)
"ceiling/Float->Word8"
    ceiling  = (fromIntegral :: Int -> Word8) . (ceiling :: Float -> Int)
"round/Float->Word8"
    round    = (fromIntegral :: Int -> Word8) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word8,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word8) n, y :: Double) }
"truncate/Double->Word8"
    truncate = (fromIntegral :: Int -> Word8) . (truncate :: Double -> Int)
"floor/Double->Word8"
    floor    = (fromIntegral :: Int -> Word8) . (floor :: Double -> Int)
"ceiling/Double->Word8"
    ceiling  = (fromIntegral :: Int -> Word8) . (ceiling :: Double -> Int)
"round/Double->Word8"
    round    = (fromIntegral :: Int -> Word8) . (round  :: Double -> Int)
  #-}

------------------------------------------------------------------------
-- type Word16
------------------------------------------------------------------------

-- Word16 is represented in the same way as Word. Operations may assume
-- and must ensure that it holds only values from its logical range.

data {-# CTYPE "HsWord16" #-} Word16 = W16# Word# deriving (Eq, Ord)
-- ^ 16-bit unsigned integer type

instance Show Word16 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word16 where
    (W16# x#) + (W16# y#)  = W16# (narrow16Word# (x# `plusWord#` y#))
    (W16# x#) - (W16# y#)  = W16# (narrow16Word# (x# `minusWord#` y#))
    (W16# x#) * (W16# y#)  = W16# (narrow16Word# (x# `timesWord#` y#))
    negate (W16# x#)       = W16# (narrow16Word# (int2Word# (negateInt# (word2Int# x#))))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W16# (narrow16Word# (integerToWord i))

instance Real Word16 where
    toRational x = toInteger x % 1

instance Enum Word16 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word16"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word16"
    toEnum i@(I# i#)
        | i >= 0 && i <= fromIntegral (maxBound::Word16)
                        = W16# (int2Word# i#)
        | otherwise     = toEnumError "Word16" i (minBound::Word16, maxBound::Word16)
    fromEnum (W16# x#)  = I# (word2Int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Word16 where
    quot    (W16# x#) y@(W16# y#)
        | y /= 0                    = W16# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    rem     (W16# x#) y@(W16# y#)
        | y /= 0                    = W16# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    div     (W16# x#) y@(W16# y#)
        | y /= 0                    = W16# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    mod     (W16# x#) y@(W16# y#)
        | y /= 0                    = W16# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    quotRem (W16# x#) y@(W16# y#)
        | y /= 0                  = case x# `quotRemWord#` y# of
                                    (# q, r #) ->
                                        (W16# q, W16# r)
        | otherwise                 = divZeroError
    divMod  (W16# x#) y@(W16# y#)
        | y /= 0                    = (W16# (x# `quotWord#` y#), W16# (x# `remWord#` y#))
        | otherwise                 = divZeroError
    toInteger (W16# x#)             = smallInteger (word2Int# x#)

instance Bounded Word16 where
    minBound = 0
    maxBound = 0xFFFF

instance Ix Word16 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Read Word16 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word16 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W16# x#) .&.   (W16# y#)  = W16# (x# `and#` y#)
    (W16# x#) .|.   (W16# y#)  = W16# (x# `or#`  y#)
    (W16# x#) `xor` (W16# y#)  = W16# (x# `xor#` y#)
    complement (W16# x#)       = W16# (x# `xor#` mb#)
        where !(W16# mb#) = maxBound
    (W16# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W16# (narrow16Word# (x# `shiftL#` i#))
        | otherwise            = W16# (x# `shiftRL#` negateInt# i#)
    (W16# x#) `shiftL` (I# i#)       = W16# (narrow16Word# (x# `shiftL#` i#))
    (W16# x#) `unsafeShiftL` (I# i#) =
        W16# (narrow16Word# (x# `uncheckedShiftL#` i#))
    (W16# x#) `shiftR`       (I# i#) = W16# (x# `shiftRL#` i#)
    (W16# x#) `unsafeShiftR` (I# i#) = W16# (x# `uncheckedShiftRL#` i#)
    (W16# x#) `rotate`       (I# i#)
        | isTrue# (i'# ==# 0#) = W16# x#
        | otherwise  = W16# (narrow16Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (16# -# i'#))))
        where
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W16# x#)        = I# (word2Int# (popCnt16# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word16 where
    finiteBitSize _ = 16

byteSwap16 :: Word16 -> Word16
byteSwap16 (W16# w#) = W16# (narrow16Word# (byteSwap16# w#))

{-# RULES
"fromIntegral/Word8->Word16"   fromIntegral = \(W8# x#) -> W16# x#
"fromIntegral/Word16->Word16"  fromIntegral = id :: Word16 -> Word16
"fromIntegral/Word16->Integer" fromIntegral = toInteger :: Word16 -> Integer
"fromIntegral/a->Word16"       fromIntegral = \x -> case fromIntegral x of W# x# -> W16# (narrow16Word# x#)
"fromIntegral/Word16->a"       fromIntegral = \(W16# x#) -> fromIntegral (W# x#)
  #-}

{-# RULES
"properFraction/Float->(Word16,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word16) n, y :: Float) }
"truncate/Float->Word16"
    truncate = (fromIntegral :: Int -> Word16) . (truncate :: Float -> Int)
"floor/Float->Word16"
    floor    = (fromIntegral :: Int -> Word16) . (floor :: Float -> Int)
"ceiling/Float->Word16"
    ceiling  = (fromIntegral :: Int -> Word16) . (ceiling :: Float -> Int)
"round/Float->Word16"
    round    = (fromIntegral :: Int -> Word16) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word16,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word16) n, y :: Double) }
"truncate/Double->Word16"
    truncate = (fromIntegral :: Int -> Word16) . (truncate :: Double -> Int)
"floor/Double->Word16"
    floor    = (fromIntegral :: Int -> Word16) . (floor :: Double -> Int)
"ceiling/Double->Word16"
    ceiling  = (fromIntegral :: Int -> Word16) . (ceiling :: Double -> Int)
"round/Double->Word16"
    round    = (fromIntegral :: Int -> Word16) . (round  :: Double -> Int)
  #-}

------------------------------------------------------------------------
-- type Word32
------------------------------------------------------------------------

-- Word32 is represented in the same way as Word.
#if WORD_SIZE_IN_BITS > 32
-- Operations may assume and must ensure that it holds only values
-- from its logical range.

-- We can use rewrite rules for the RealFrac methods

{-# RULES
"properFraction/Float->(Word32,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word32) n, y :: Float) }
"truncate/Float->Word32"
    truncate = (fromIntegral :: Int -> Word32) . (truncate :: Float -> Int)
"floor/Float->Word32"
    floor    = (fromIntegral :: Int -> Word32) . (floor :: Float -> Int)
"ceiling/Float->Word32"
    ceiling  = (fromIntegral :: Int -> Word32) . (ceiling :: Float -> Int)
"round/Float->Word32"
    round    = (fromIntegral :: Int -> Word32) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word32,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word32) n, y :: Double) }
"truncate/Double->Word32"
    truncate = (fromIntegral :: Int -> Word32) . (truncate :: Double -> Int)
"floor/Double->Word32"
    floor    = (fromIntegral :: Int -> Word32) . (floor :: Double -> Int)
"ceiling/Double->Word32"
    ceiling  = (fromIntegral :: Int -> Word32) . (ceiling :: Double -> Int)
"round/Double->Word32"
    round    = (fromIntegral :: Int -> Word32) . (round  :: Double -> Int)
  #-}

#endif

data {-# CTYPE "HsWord32" #-} Word32 = W32# Word# deriving (Eq, Ord)
-- ^ 32-bit unsigned integer type

instance Num Word32 where
    (W32# x#) + (W32# y#)  = W32# (narrow32Word# (x# `plusWord#` y#))
    (W32# x#) - (W32# y#)  = W32# (narrow32Word# (x# `minusWord#` y#))
    (W32# x#) * (W32# y#)  = W32# (narrow32Word# (x# `timesWord#` y#))
    negate (W32# x#)       = W32# (narrow32Word# (int2Word# (negateInt# (word2Int# x#))))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W32# (narrow32Word# (integerToWord i))

instance Enum Word32 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word32"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word32"
    toEnum i@(I# i#)
        | i >= 0
#if WORD_SIZE_IN_BITS > 32
          && i <= fromIntegral (maxBound::Word32)
#endif
                        = W32# (int2Word# i#)
        | otherwise     = toEnumError "Word32" i (minBound::Word32, maxBound::Word32)
#if WORD_SIZE_IN_BITS == 32
    fromEnum x@(W32# x#)
        | x <= fromIntegral (maxBound::Int)
                        = I# (word2Int# x#)
        | otherwise     = fromEnumError "Word32" x
    enumFrom            = integralEnumFrom
    enumFromThen        = integralEnumFromThen
    enumFromTo          = integralEnumFromTo
    enumFromThenTo      = integralEnumFromThenTo
#else
    fromEnum (W32# x#)  = I# (word2Int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen
#endif

instance Integral Word32 where
    quot    (W32# x#) y@(W32# y#)
        | y /= 0                    = W32# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    rem     (W32# x#) y@(W32# y#)
        | y /= 0                    = W32# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    div     (W32# x#) y@(W32# y#)
        | y /= 0                    = W32# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    mod     (W32# x#) y@(W32# y#)
        | y /= 0                    = W32# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    quotRem (W32# x#) y@(W32# y#)
        | y /= 0                  = case x# `quotRemWord#` y# of
                                    (# q, r #) ->
                                        (W32# q, W32# r)
        | otherwise                 = divZeroError
    divMod  (W32# x#) y@(W32# y#)
        | y /= 0                    = (W32# (x# `quotWord#` y#), W32# (x# `remWord#` y#))
        | otherwise                 = divZeroError
    toInteger (W32# x#)
#if WORD_SIZE_IN_BITS == 32
        | isTrue# (i# >=# 0#)       = smallInteger i#
        | otherwise                 = wordToInteger x#
        where
        !i# = word2Int# x#
#else
                                    = smallInteger (word2Int# x#)
#endif

instance Bits Word32 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W32# x#) .&.   (W32# y#)  = W32# (x# `and#` y#)
    (W32# x#) .|.   (W32# y#)  = W32# (x# `or#`  y#)
    (W32# x#) `xor` (W32# y#)  = W32# (x# `xor#` y#)
    complement (W32# x#)       = W32# (x# `xor#` mb#)
        where !(W32# mb#) = maxBound
    (W32# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W32# (narrow32Word# (x# `shiftL#` i#))
        | otherwise            = W32# (x# `shiftRL#` negateInt# i#)
    (W32# x#) `shiftL`       (I# i#) = W32# (narrow32Word# (x# `shiftL#` i#))
    (W32# x#) `unsafeShiftL` (I# i#) =
        W32# (narrow32Word# (x# `uncheckedShiftL#` i#))
    (W32# x#) `shiftR`       (I# i#) = W32# (x# `shiftRL#` i#)
    (W32# x#) `unsafeShiftR` (I# i#) = W32# (x# `uncheckedShiftRL#` i#)
    (W32# x#) `rotate`       (I# i#)
        | isTrue# (i'# ==# 0#) = W32# x#
        | otherwise   = W32# (narrow32Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (32# -# i'#))))
        where
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W32# x#)        = I# (word2Int# (popCnt32# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word32 where
    finiteBitSize _ = 32

{-# RULES
"fromIntegral/Word8->Word32"   fromIntegral = \(W8# x#) -> W32# x#
"fromIntegral/Word16->Word32"  fromIntegral = \(W16# x#) -> W32# x#
"fromIntegral/Word32->Word32"  fromIntegral = id :: Word32 -> Word32
"fromIntegral/Word32->Integer" fromIntegral = toInteger :: Word32 -> Integer
"fromIntegral/a->Word32"       fromIntegral = \x -> case fromIntegral x of W# x# -> W32# (narrow32Word# x#)
"fromIntegral/Word32->a"       fromIntegral = \(W32# x#) -> fromIntegral (W# x#)
  #-}

instance Show Word32 where
#if WORD_SIZE_IN_BITS < 33
    showsPrec p x = showsPrec p (toInteger x)
#else
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
#endif


instance Real Word32 where
    toRational x = toInteger x % 1

instance Bounded Word32 where
    minBound = 0
    maxBound = 0xFFFFFFFF

instance Ix Word32 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Read Word32 where
#if WORD_SIZE_IN_BITS < 33
    readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s]
#else
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]
#endif

byteSwap32 :: Word32 -> Word32
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))

------------------------------------------------------------------------
-- type Word64
------------------------------------------------------------------------

#if WORD_SIZE_IN_BITS < 64

data {-# CTYPE "HsWord64" #-} Word64 = W64# Word64#
-- ^ 64-bit unsigned integer type

instance Eq Word64 where
    (W64# x#) == (W64# y#) = isTrue# (x# `eqWord64#` y#)
    (W64# x#) /= (W64# y#) = isTrue# (x# `neWord64#` y#)

instance Ord Word64 where
    (W64# x#) <  (W64# y#) = isTrue# (x# `ltWord64#` y#)
    (W64# x#) <= (W64# y#) = isTrue# (x# `leWord64#` y#)
    (W64# x#) >  (W64# y#) = isTrue# (x# `gtWord64#` y#)
    (W64# x#) >= (W64# y#) = isTrue# (x# `geWord64#` y#)

instance Num Word64 where
    (W64# x#) + (W64# y#)  = W64# (int64ToWord64# (word64ToInt64# x# `plusInt64#` word64ToInt64# y#))
    (W64# x#) - (W64# y#)  = W64# (int64ToWord64# (word64ToInt64# x# `minusInt64#` word64ToInt64# y#))
    (W64# x#) * (W64# y#)  = W64# (int64ToWord64# (word64ToInt64# x# `timesInt64#` word64ToInt64# y#))
    negate (W64# x#)       = W64# (int64ToWord64# (negateInt64# (word64ToInt64# x#)))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W64# (integerToWord64 i)

instance Enum Word64 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word64"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word64"
    toEnum i@(I# i#)
        | i >= 0        = W64# (wordToWord64# (int2Word# i#))
        | otherwise     = toEnumError "Word64" i (minBound::Word64, maxBound::Word64)
    fromEnum x@(W64# x#)
        | x <= fromIntegral (maxBound::Int)
                        = I# (word2Int# (word64ToWord# x#))
        | otherwise     = fromEnumError "Word64" x
    enumFrom            = integralEnumFrom
    enumFromThen        = integralEnumFromThen
    enumFromTo          = integralEnumFromTo
    enumFromThenTo      = integralEnumFromThenTo

instance Integral Word64 where
    quot    (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `quotWord64#` y#)
        | otherwise                 = divZeroError
    rem     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `remWord64#` y#)
        | otherwise                 = divZeroError
    div     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `quotWord64#` y#)
        | otherwise                 = divZeroError
    mod     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `remWord64#` y#)
        | otherwise                 = divZeroError
    quotRem (W64# x#) y@(W64# y#)
        | y /= 0                    = (W64# (x# `quotWord64#` y#), W64# (x# `remWord64#` y#))
        | otherwise                 = divZeroError
    divMod  (W64# x#) y@(W64# y#)
        | y /= 0                    = (W64# (x# `quotWord64#` y#), W64# (x# `remWord64#` y#))
        | otherwise                 = divZeroError
    toInteger (W64# x#)             = word64ToInteger x#

instance Bits Word64 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W64# x#) .&.   (W64# y#)  = W64# (x# `and64#` y#)
    (W64# x#) .|.   (W64# y#)  = W64# (x# `or64#`  y#)
    (W64# x#) `xor` (W64# y#)  = W64# (x# `xor64#` y#)
    complement (W64# x#)       = W64# (not64# x#)
    (W64# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W64# (x# `shiftL64#` i#)
        | otherwise            = W64# (x# `shiftRL64#` negateInt# i#)
    (W64# x#) `shiftL`       (I# i#) = W64# (x# `shiftL64#` i#)
    (W64# x#) `unsafeShiftL` (I# i#) = W64# (x# `uncheckedShiftL64#` i#)
    (W64# x#) `shiftR`       (I# i#) = W64# (x# `shiftRL64#` i#)
    (W64# x#) `unsafeShiftR` (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)
    (W64# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise            = W64# ((x# `uncheckedShiftL64#` i'#) `or64#`
                                       (x# `uncheckedShiftRL64#` (64# -# i'#)))
        where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W64# x#)        = I# (word2Int# (popCnt64# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

-- give the 64-bit shift operations the same treatment as the 32-bit
-- ones (see GHC.Base), namely we wrap them in tests to catch the
-- cases when we're shifting more than 64 bits to avoid unspecified
-- behaviour in the C shift operations.

shiftL64#, shiftRL64# :: Word64# -> Int# -> Word64#

a `shiftL64#` b  | isTrue# (b >=# 64#) = wordToWord64# 0##
                 | otherwise           = a `uncheckedShiftL64#` b

a `shiftRL64#` b | isTrue# (b >=# 64#) = wordToWord64# 0##
                 | otherwise           = a `uncheckedShiftRL64#` b

{-# RULES
"fromIntegral/Int->Word64"    fromIntegral = \(I#   x#) -> W64# (int64ToWord64# (intToInt64# x#))
"fromIntegral/Word->Word64"   fromIntegral = \(W#   x#) -> W64# (wordToWord64# x#)
"fromIntegral/Word64->Int"    fromIntegral = \(W64# x#) -> I#   (word2Int# (word64ToWord# x#))
"fromIntegral/Word64->Word"   fromIntegral = \(W64# x#) -> W#   (word64ToWord# x#)
"fromIntegral/Word64->Word64" fromIntegral = id :: Word64 -> Word64
  #-}

#else

-- Word64 is represented in the same way as Word.
-- Operations may assume and must ensure that it holds only values
-- from its logical range.

data {-# CTYPE "HsWord64" #-} Word64 = W64# Word# deriving (Eq, Ord)
-- ^ 64-bit unsigned integer type

instance Num Word64 where
    (W64# x#) + (W64# y#)  = W64# (x# `plusWord#` y#)
    (W64# x#) - (W64# y#)  = W64# (x# `minusWord#` y#)
    (W64# x#) * (W64# y#)  = W64# (x# `timesWord#` y#)
    negate (W64# x#)       = W64# (int2Word# (negateInt# (word2Int# x#)))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W64# (integerToWord i)

instance Enum Word64 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word64"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word64"
    toEnum i@(I# i#)
        | i >= 0        = W64# (int2Word# i#)
        | otherwise     = toEnumError "Word64" i (minBound::Word64, maxBound::Word64)
    fromEnum x@(W64# x#)
        | x <= fromIntegral (maxBound::Int)
                        = I# (word2Int# x#)
        | otherwise     = fromEnumError "Word64" x
    enumFrom            = integralEnumFrom
    enumFromThen        = integralEnumFromThen
    enumFromTo          = integralEnumFromTo
    enumFromThenTo      = integralEnumFromThenTo

instance Integral Word64 where
    quot    (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    rem     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    div     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    mod     (W64# x#) y@(W64# y#)
        | y /= 0                    = W64# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    quotRem (W64# x#) y@(W64# y#)
        | y /= 0                  = case x# `quotRemWord#` y# of
                                    (# q, r #) ->
                                        (W64# q, W64# r)
        | otherwise                 = divZeroError
    divMod  (W64# x#) y@(W64# y#)
        | y /= 0                    = (W64# (x# `quotWord#` y#), W64# (x# `remWord#` y#))
        | otherwise                 = divZeroError
    toInteger (W64# x#)
        | isTrue# (i# >=# 0#)       = smallInteger i#
        | otherwise                 = wordToInteger x#
        where
        !i# = word2Int# x#

instance Bits Word64 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W64# x#) .&.   (W64# y#)  = W64# (x# `and#` y#)
    (W64# x#) .|.   (W64# y#)  = W64# (x# `or#`  y#)
    (W64# x#) `xor` (W64# y#)  = W64# (x# `xor#` y#)
    complement (W64# x#)       = W64# (x# `xor#` mb#)
        where !(W64# mb#) = maxBound
    (W64# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W64# (x# `shiftL#` i#)
        | otherwise            = W64# (x# `shiftRL#` negateInt# i#)
    (W64# x#) `shiftL`       (I# i#) = W64# (x# `shiftL#` i#)
    (W64# x#) `unsafeShiftL` (I# i#) = W64# (x# `uncheckedShiftL#` i#)
    (W64# x#) `shiftR`       (I# i#) = W64# (x# `shiftRL#` i#)
    (W64# x#) `unsafeShiftR` (I# i#) = W64# (x# `uncheckedShiftRL#` i#)
    (W64# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise            = W64# ((x# `uncheckedShiftL#` i'#) `or#`
                                       (x# `uncheckedShiftRL#` (64# -# i'#)))
        where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W64# x#)        = I# (word2Int# (popCnt64# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

{-# RULES
"fromIntegral/a->Word64" fromIntegral = \x -> case fromIntegral x of W# x# -> W64# x#
"fromIntegral/Word64->a" fromIntegral = \(W64# x#) -> fromIntegral (W# x#)
  #-}

uncheckedShiftL64# :: Word# -> Int# -> Word#
uncheckedShiftL64#  = uncheckedShiftL#

uncheckedShiftRL64# :: Word# -> Int# -> Word#
uncheckedShiftRL64# = uncheckedShiftRL#

#endif

instance FiniteBits Word64 where
    finiteBitSize _ = 64

instance Show Word64 where
    showsPrec p x = showsPrec p (toInteger x)

instance Real Word64 where
    toRational x = toInteger x % 1

instance Bounded Word64 where
    minBound = 0
    maxBound = 0xFFFFFFFFFFFFFFFF

instance Ix Word64 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Read Word64 where
    readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s]

#if WORD_SIZE_IN_BITS < 64
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# w#) = W64# (byteSwap64# w#)
#else
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# w#) = W64# (byteSwap# w#)
#endif
