%
% (c) The University of Glasgow, 1997-2001
%
\section[GHC.Int]{Module @GHC.Int@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

#include "MachDeps.h"

module GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..))
    where

import Data.Bits

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Word
import GHC.Show

------------------------------------------------------------------------
-- type Int8
------------------------------------------------------------------------

-- Int8 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data Int8 = I8# Int# deriving (Eq, Ord)

instance CCallable Int8
instance CReturnable Int8

instance Show Int8 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int8 where
    (I8# x#) + (I8# y#)    = I8# (intToInt8# (x# +# y#))
    (I8# x#) - (I8# y#)    = I8# (intToInt8# (x# -# y#))
    (I8# x#) * (I8# y#)    = I8# (intToInt8# (x# *# y#))
    negate (I8# x#)        = I8# (intToInt8# (negateInt# x#))
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger (S# i#)    = I8# (intToInt8# i#)
    fromInteger (J# s# d#) = I8# (intToInt8# (integer2Int# s# d#))

instance Real Int8 where
    toRational x = toInteger x % 1

instance Enum Int8 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int8"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int8"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int8) && i <= fromIntegral (maxBound::Int8)
                        = I8# i#
        | otherwise     = toEnumError "Int8" i (minBound::Int8, maxBound::Int8)
    fromEnum (I8# x#)   = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int8 where
    quot    x@(I8# x#) y@(I8# y#)
        | y /= 0                  = I8# (intToInt8# (x# `quotInt#` y#))
        | otherwise               = divZeroError "quot{Int8}" x
    rem     x@(I8# x#) y@(I8# y#)
        | y /= 0                  = I8# (intToInt8# (x# `remInt#` y#))
        | otherwise               = divZeroError "rem{Int8}" x
    div     x@(I8# x#) y@(I8# y#)
        | y /= 0                  = I8# (intToInt8# (x# `divInt#` y#))
        | otherwise               = divZeroError "div{Int8}" x
    mod     x@(I8# x#) y@(I8# y#)
        | y /= 0                  = I8# (intToInt8# (x# `modInt#` y#))
        | otherwise               = divZeroError "mod{Int8}" x
    quotRem x@(I8# x#) y@(I8# y#)
        | y /= 0                  = (I8# (intToInt8# (x# `quotInt#` y#)),
                                    I8# (intToInt8# (x# `remInt#` y#)))
        | otherwise               = divZeroError "quotRem{Int8}" x
    divMod  x@(I8# x#) y@(I8# y#)
        | y /= 0                  = (I8# (intToInt8# (x# `divInt#` y#)),
                                    I8# (intToInt8# (x# `modInt#` y#)))
        | otherwise               = divZeroError "divMod{Int8}" x
    toInteger (I8# x#)            = S# x#

instance Bounded Int8 where
    minBound = -0x80
    maxBound =  0x7F

instance Ix Int8 where
    range (m,n)       = [m..n]
    index b@(m,_) i
        | inRange b i = fromIntegral (i - m)
        | otherwise   = indexError b i "Int8"
    inRange (m,n) i   = m <= i && i <= n

instance Read Int8 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int8 where
    (I8# x#) .&.   (I8# y#)   = I8# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I8# x#) .|.   (I8# y#)   = I8# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I8# x#) `xor` (I8# y#)   = I8# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I8# x#)       = I8# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I8# x#) `shift` (I# i#)
        | i# >=# 0#           = I8# (intToInt8# (x# `iShiftL#` i#))
        | otherwise           = I8# (x# `iShiftRA#` negateInt# i#)
    (I8# x#) `rotate` (I# i#) =
        I8# (intToInt8# (word2Int# ((x'# `shiftL#` i'#) `or#`
                                    (x'# `shiftRL#` (8# -# i'#)))))
        where
        x'# = wordToWord8# (int2Word# x#)
        i'# = word2Int# (int2Word# i# `and#` int2Word# 7#)
    bitSize  _                = 8
    isSigned _                = True

{-# RULES
"fromIntegral/Int8->Int8" fromIntegral = id :: Int8 -> Int8
"fromIntegral/a->Int8"    fromIntegral = \x -> case fromIntegral x of I# x# -> I8# (intToInt8# x#)
"fromIntegral/Int8->a"    fromIntegral = \(I8# x#) -> fromIntegral (I# x#)
  #-}

------------------------------------------------------------------------
-- type Int16
------------------------------------------------------------------------

-- Int16 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data Int16 = I16# Int# deriving (Eq, Ord)

instance CCallable Int16
instance CReturnable Int16

instance Show Int16 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int16 where
    (I16# x#) + (I16# y#)  = I16# (intToInt16# (x# +# y#))
    (I16# x#) - (I16# y#)  = I16# (intToInt16# (x# -# y#))
    (I16# x#) * (I16# y#)  = I16# (intToInt16# (x# *# y#))
    negate (I16# x#)       = I16# (intToInt16# (negateInt# x#))
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger (S# i#)    = I16# (intToInt16# i#)
    fromInteger (J# s# d#) = I16# (intToInt16# (integer2Int# s# d#))

instance Real Int16 where
    toRational x = toInteger x % 1

instance Enum Int16 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int16"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int16"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int16) && i <= fromIntegral (maxBound::Int16)
                        = I16# i#
        | otherwise     = toEnumError "Int16" i (minBound::Int16, maxBound::Int16)
    fromEnum (I16# x#)  = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int16 where
    quot    x@(I16# x#) y@(I16# y#)
        | y /= 0                  = I16# (intToInt16# (x# `quotInt#` y#))
        | otherwise               = divZeroError "quot{Int16}" x
    rem     x@(I16# x#) y@(I16# y#)
        | y /= 0                  = I16# (intToInt16# (x# `remInt#` y#))
        | otherwise               = divZeroError "rem{Int16}" x
    div     x@(I16# x#) y@(I16# y#)
        | y /= 0                  = I16# (intToInt16# (x# `divInt#` y#))
        | otherwise               = divZeroError "div{Int16}" x
    mod     x@(I16# x#) y@(I16# y#)
        | y /= 0                  = I16# (intToInt16# (x# `modInt#` y#))
        | otherwise               = divZeroError "mod{Int16}" x
    quotRem x@(I16# x#) y@(I16# y#)
        | y /= 0                  = (I16# (intToInt16# (x# `quotInt#` y#)),
                                    I16# (intToInt16# (x# `remInt#` y#)))
        | otherwise               = divZeroError "quotRem{Int16}" x
    divMod  x@(I16# x#) y@(I16# y#)
        | y /= 0                  = (I16# (intToInt16# (x# `divInt#` y#)),
                                    I16# (intToInt16# (x# `modInt#` y#)))
        | otherwise               = divZeroError "divMod{Int16}" x
    toInteger (I16# x#)           = S# x#

instance Bounded Int16 where
    minBound = -0x8000
    maxBound =  0x7FFF

instance Ix Int16 where
    range (m,n)       = [m..n]
    index b@(m,_) i
        | inRange b i = fromIntegral (i - m)
        | otherwise   = indexError b i "Int16"
    inRange (m,n) i   = m <= i && i <= n

instance Read Int16 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int16 where
    (I16# x#) .&.   (I16# y#)  = I16# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I16# x#) .|.   (I16# y#)  = I16# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I16# x#) `xor` (I16# y#)  = I16# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I16# x#)       = I16# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I16# x#) `shift` (I# i#)
        | i# >=# 0#            = I16# (intToInt16# (x# `iShiftL#` i#))
        | otherwise            = I16# (x# `iShiftRA#` negateInt# i#)
    (I16# x#) `rotate` (I# i#) =
        I16# (intToInt16# (word2Int# ((x'# `shiftL#` i'#) `or#`
                                      (x'# `shiftRL#` (16# -# i'#)))))
        where
        x'# = wordToWord16# (int2Word# x#)
        i'# = word2Int# (int2Word# i# `and#` int2Word# 15#)
    bitSize  _                 = 16
    isSigned _                 = True

{-# RULES
"fromIntegral/Word8->Int16"  fromIntegral = \(W8# x#) -> I16# (word2Int# x#)
"fromIntegral/Int8->Int16"   fromIntegral = \(I8# x#) -> I16# x#
"fromIntegral/Int16->Int16"  fromIntegral = id :: Int16 -> Int16
"fromIntegral/a->Int16"      fromIntegral = \x -> case fromIntegral x of I# x# -> I16# (intToInt16# x#)
"fromIntegral/Int16->a"      fromIntegral = \(I16# x#) -> fromIntegral (I# x#)
  #-}

------------------------------------------------------------------------
-- type Int32
------------------------------------------------------------------------

-- Int32 is represented in the same way as Int.
#if WORD_SIZE_IN_BYTES == 8
-- Operations may assume and must ensure that it holds only values
-- from its logical range.
#endif

data Int32 = I32# Int# deriving (Eq, Ord)

instance CCallable Int32
instance CReturnable Int32

instance Show Int32 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int32 where
    (I32# x#) + (I32# y#)  = I32# (intToInt32# (x# +# y#))
    (I32# x#) - (I32# y#)  = I32# (intToInt32# (x# -# y#))
    (I32# x#) * (I32# y#)  = I32# (intToInt32# (x# *# y#))
    negate (I32# x#)       = I32# (intToInt32# (negateInt# x#))
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger (S# i#)    = I32# (intToInt32# i#)
    fromInteger (J# s# d#) = I32# (intToInt32# (integer2Int# s# d#))

instance Real Int32 where
    toRational x = toInteger x % 1

instance Enum Int32 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int32"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int32"
#if WORD_SIZE_IN_BYTES == 4
    toEnum (I# i#)      = I32# i#
#else
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int32) && i <= fromIntegral (maxBound::Int32)
                        = I32# i#
        | otherwise     = toEnumError "Int32" i (minBound::Int32, maxBound::Int32)
#endif
    fromEnum (I32# x#)  = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int32 where
    quot    x@(I32# x#) y@(I32# y#)
        | y /= 0                  = I32# (intToInt32# (x# `quotInt#` y#))
        | otherwise               = divZeroError "quot{Int32}" x
    rem     x@(I32# x#) y@(I32# y#)
        | y /= 0                  = I32# (intToInt32# (x# `remInt#` y#))
        | otherwise               = divZeroError "rem{Int32}" x
    div     x@(I32# x#) y@(I32# y#)
        | y /= 0                  = I32# (intToInt32# (x# `divInt#` y#))
        | otherwise               = divZeroError "div{Int32}" x
    mod     x@(I32# x#) y@(I32# y#)
        | y /= 0                  = I32# (intToInt32# (x# `modInt#` y#))
        | otherwise               = divZeroError "mod{Int32}" x
    quotRem x@(I32# x#) y@(I32# y#)
        | y /= 0                  = (I32# (intToInt32# (x# `quotInt#` y#)),
                                    I32# (intToInt32# (x# `remInt#` y#)))
        | otherwise               = divZeroError "quotRem{Int32}" x
    divMod  x@(I32# x#) y@(I32# y#)
        | y /= 0                  = (I32# (intToInt32# (x# `divInt#` y#)),
                                    I32# (intToInt32# (x# `modInt#` y#)))
        | otherwise               = divZeroError "divMod{Int32}" x
    toInteger (I32# x#)           = S# x#

instance Bounded Int32 where
    minBound = -0x80000000
    maxBound =  0x7FFFFFFF

instance Ix Int32 where
    range (m,n)       = [m..n]
    index b@(m,_) i
        | inRange b i = fromIntegral (i - m)
        | otherwise   = indexError b i "Int32"
    inRange (m,n) i   = m <= i && i <= n

instance Read Int32 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int32 where
    (I32# x#) .&.   (I32# y#)  = I32# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I32# x#) .|.   (I32# y#)  = I32# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I32# x#) `xor` (I32# y#)  = I32# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I32# x#)       = I32# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I32# x#) `shift` (I# i#)
        | i# >=# 0#            = I32# (intToInt32# (x# `iShiftL#` i#))
        | otherwise            = I32# (x# `iShiftRA#` negateInt# i#)
    (I32# x#) `rotate` (I# i#) =
        I32# (intToInt32# (word2Int# ((x'# `shiftL#` i'#) `or#`
                                      (x'# `shiftRL#` (32# -# i'#)))))
        where
        x'# = wordToWord32# (int2Word# x#)
        i'# = word2Int# (int2Word# i# `and#` int2Word# 31#)
    bitSize  _                 = 32
    isSigned _                 = True

{-# RULES
"fromIntegral/Word8->Int32"  fromIntegral = \(W8# x#) -> I32# (word2Int# x#)
"fromIntegral/Word16->Int32" fromIntegral = \(W16# x#) -> I32# (word2Int# x#)
"fromIntegral/Int8->Int32"   fromIntegral = \(I8# x#) -> I32# x#
"fromIntegral/Int16->Int32"  fromIntegral = \(I16# x#) -> I32# x#
"fromIntegral/Int32->Int32"  fromIntegral = id :: Int32 -> Int32
"fromIntegral/a->Int32"      fromIntegral = \x -> case fromIntegral x of I# x# -> I32# (intToInt32# x#)
"fromIntegral/Int32->a"      fromIntegral = \(I32# x#) -> fromIntegral (I# x#)
  #-}

------------------------------------------------------------------------
-- type Int64
------------------------------------------------------------------------

#if WORD_SIZE_IN_BYTES == 4

data Int64 = I64# Int64#

instance Eq Int64 where
    (I64# x#) == (I64# y#) = x# `eqInt64#` y#
    (I64# x#) /= (I64# y#) = x# `neInt64#` y#

instance Ord Int64 where
    (I64# x#) <  (I64# y#) = x# `ltInt64#` y#
    (I64# x#) <= (I64# y#) = x# `leInt64#` y#
    (I64# x#) >  (I64# y#) = x# `gtInt64#` y#
    (I64# x#) >= (I64# y#) = x# `geInt64#` y#

instance Show Int64 where
    showsPrec p x = showsPrec p (toInteger x)

instance Num Int64 where
    (I64# x#) + (I64# y#)  = I64# (x# `plusInt64#`  y#)
    (I64# x#) - (I64# y#)  = I64# (x# `minusInt64#` y#)
    (I64# x#) * (I64# y#)  = I64# (x# `timesInt64#` y#)
    negate (I64# x#)       = I64# (negateInt64# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger (S# i#)    = I64# (intToInt64# i#)
    fromInteger (J# s# d#) = I64# (integerToInt64# s# d#)

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
    enumFrom            = integralEnumFrom
    enumFromThen        = integralEnumFromThen
    enumFromTo          = integralEnumFromTo
    enumFromThenTo      = integralEnumFromThenTo

instance Integral Int64 where
    quot    x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `quotInt64#` y#)
        | otherwise               = divZeroError "quot{Int64}" x
    rem     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `remInt64#` y#)
        | otherwise               = divZeroError "rem{Int64}" x
    div     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `divInt64#` y#)
        | otherwise               = divZeroError "div{Int64}" x
    mod     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `modInt64#` y#)
        | otherwise               = divZeroError "mod{Int64}" x
    quotRem x@(I64# x#) y@(I64# y#)
        | y /= 0                  = (I64# (x# `quotInt64#` y#), I64# (x# `remInt64#` y#))
        | otherwise               = divZeroError "quotRem{Int64}" x
    divMod  x@(I64# x#) y@(I64# y#)
        | y /= 0                  = (I64# (x# `divInt64#` y#), I64# (x# `modInt64#` y#))
        | otherwise               = divZeroError "divMod{Int64}" x
    toInteger x@(I64# x#)
        | x >= -0x80000000 && x <= 0x7FFFFFFF
                                  = S# (int64ToInt# x#)
        | otherwise               = case int64ToInteger# x# of (# s, d #) -> J# s d

divInt64#, modInt64# :: Int64# -> Int64# -> Int64#
x# `divInt64#` y#
    | (x# `gtInt64#` intToInt64# 0#) && (y# `ltInt64#` intToInt64# 0#)
        = ((x# `minusInt64#` y#) `minusInt64#` intToInt64# 1#) `quotInt64#` y#
    | (x# `ltInt64#` intToInt64# 0#) && (y# `gtInt64#` intToInt64# 0#)
        = ((x# `minusInt64#` y#) `plusInt64#` intToInt64# 1#) `quotInt64#` y#
    | otherwise                = x# `quotInt64#` y#
x# `modInt64#` y#
    | (x# `gtInt64#` intToInt64# 0#) && (y# `ltInt64#` intToInt64# 0#) ||
      (x# `ltInt64#` intToInt64# 0#) && (y# `gtInt64#` intToInt64# 0#)
        = if r# `neInt64#` intToInt64# 0# then r# `plusInt64#` y# else intToInt64# 0#
    | otherwise = r#
    where
    r# = x# `remInt64#` y#

instance Read Int64 where
    readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s]

instance Bits Int64 where
    (I64# x#) .&.   (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `and64#` int64ToWord64# y#))
    (I64# x#) .|.   (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `or64#`  int64ToWord64# y#))
    (I64# x#) `xor` (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` int64ToWord64# y#))
    complement (I64# x#)       = I64# (word64ToInt64# (not64# (int64ToWord64# x#)))
    (I64# x#) `shift` (I# i#)
        | i# >=# 0#            = I64# (x# `iShiftL64#` i#)
        | otherwise            = I64# (x# `iShiftRA64#` negateInt# i#)
    (I64# x#) `rotate` (I# i#) =
        I64# (word64ToInt64# ((x'# `shiftL64#` i'#) `or64#`
                              (x'# `shiftRL64#` (64# -# i'#))))
        where
        x'# = int64ToWord64# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# 63#)
    bitSize  _                 = 64
    isSigned _                 = True

foreign import "stg_eqInt64"       unsafe eqInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_neInt64"       unsafe neInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_ltInt64"       unsafe ltInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_leInt64"       unsafe leInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_gtInt64"       unsafe gtInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_geInt64"       unsafe geInt64#       :: Int64# -> Int64# -> Bool
foreign import "stg_plusInt64"     unsafe plusInt64#     :: Int64# -> Int64# -> Int64#
foreign import "stg_minusInt64"    unsafe minusInt64#    :: Int64# -> Int64# -> Int64#
foreign import "stg_timesInt64"    unsafe timesInt64#    :: Int64# -> Int64# -> Int64#
foreign import "stg_negateInt64"   unsafe negateInt64#   :: Int64# -> Int64#
foreign import "stg_quotInt64"     unsafe quotInt64#     :: Int64# -> Int64# -> Int64#
foreign import "stg_remInt64"      unsafe remInt64#      :: Int64# -> Int64# -> Int64#
foreign import "stg_intToInt64"    unsafe intToInt64#    :: Int# -> Int64#
foreign import "stg_int64ToInt"    unsafe int64ToInt#    :: Int64# -> Int#
foreign import "stg_wordToWord64"  unsafe wordToWord64#  :: Word# -> Word64#
foreign import "stg_int64ToWord64" unsafe int64ToWord64# :: Int64# -> Word64#
foreign import "stg_word64ToInt64" unsafe word64ToInt64# :: Word64# -> Int64#
foreign import "stg_and64"         unsafe and64#         :: Word64# -> Word64# -> Word64#
foreign import "stg_or64"          unsafe or64#          :: Word64# -> Word64# -> Word64#
foreign import "stg_xor64"         unsafe xor64#         :: Word64# -> Word64# -> Word64#
foreign import "stg_not64"         unsafe not64#         :: Word64# -> Word64#
foreign import "stg_iShiftL64"     unsafe iShiftL64#     :: Int64# -> Int# -> Int64#
foreign import "stg_iShiftRA64"    unsafe iShiftRA64#    :: Int64# -> Int# -> Int64#
foreign import "stg_shiftL64"      unsafe shiftL64#      :: Word64# -> Int# -> Word64#
foreign import "stg_shiftRL64"     unsafe shiftRL64#     :: Word64# -> Int# -> Word64#

{-# RULES
"fromIntegral/Int->Int64"    fromIntegral = \(I#   x#) -> I64# (intToInt64# x#)
"fromIntegral/Word->Int64"   fromIntegral = \(W#   x#) -> I64# (word64ToInt64# (wordToWord64# x#))
"fromIntegral/Word64->Int64" fromIntegral = \(W64# x#) -> I64# (word64ToInt64# x#)
"fromIntegral/Int64->Int"    fromIntegral = \(I64# x#) -> I#   (int64ToInt# x#)
"fromIntegral/Int64->Word"   fromIntegral = \(I64# x#) -> W#   (int2Word# (int64ToInt# x#))
"fromIntegral/Int64->Word64" fromIntegral = \(I64# x#) -> W64# (int64ToWord64# x#)
"fromIntegral/Int64->Int64"  fromIntegral = id :: Int64 -> Int64
  #-}

#else

data Int64 = I64# Int# deriving (Eq, Ord)

instance Show Int64 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int64 where
    (I64# x#) + (I64# y#)  = I64# (x# +# y#)
    (I64# x#) - (I64# y#)  = I64# (x# -# y#)
    (I64# x#) * (I64# y#)  = I64# (x# *# y#)
    negate (I64# x#)       = I64# (negateInt# x#)
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger (S# i#)    = I64# i#
    fromInteger (J# s# d#) = I64# (integer2Int# s# d#)

instance Enum Int64 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int64"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int64"
    toEnum (I# i#)      = I64# i#
    fromEnum (I64# x#)  = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int64 where
    quot    x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `quotInt#` y#)
        | otherwise               = divZeroError "quot{Int64}" x
    rem     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `remInt#` y#)
        | otherwise               = divZeroError "rem{Int64}" x
    div     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `divInt#` y#)
        | otherwise               = divZeroError "div{Int64}" x
    mod     x@(I64# x#) y@(I64# y#)
        | y /= 0                  = I64# (x# `modInt#` y#)
        | otherwise               = divZeroError "mod{Int64}" x
    quotRem x@(I64# x#) y@(I64# y#)
        | y /= 0                  = (I64# (x# `quotInt#` y#), I64# (x# `remInt#` y#))
        | otherwise               = divZeroError "quotRem{Int64}" x
    divMod  x@(I64# x#) y@(I64# y#)
        | y /= 0                  = (I64# (x# `divInt#` y#), I64# (x# `modInt#` y#))
        | otherwise               = divZeroError "divMod{Int64}" x
    toInteger (I64# x#)           = S# x#

instance Read Int64 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int64 where
    (I64# x#) .&.   (I64# y#)  = I64# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I64# x#) .|.   (I64# y#)  = I64# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I64# x#) `xor` (I64# y#)  = I64# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I64# x#)       = I64# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I64# x#) `shift` (I# i#)
        | i# >=# 0#            = I64# (x# `iShiftL#` i#)
        | otherwise            = I64# (x# `iShiftRA#` negateInt# i#)
    (I64# x#) `rotate` (I# i#) =
        I64# (word2Int# ((x'# `shiftL#` i'#) `or#`
                         (x'# `shiftRL#` (64# -# i'#))))
        where
        x'# = int2Word# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# 63#)
    bitSize  _                 = 64
    isSigned _                 = True

{-# RULES
"fromIntegral/a->Int64" fromIntegral = \x -> case fromIntegral x of I# x# -> I64# (intToInt64# x#)
"fromIntegral/Int64->a" fromIntegral = \(I64# x#) -> fromIntegral (I# x#)
  #-}

#endif

instance CCallable Int64
instance CReturnable Int64

instance Real Int64 where
    toRational x = toInteger x % 1

instance Bounded Int64 where
    minBound = -0x8000000000000000
    maxBound =  0x7FFFFFFFFFFFFFFF

instance Ix Int64 where
    range (m,n)       = [m..n]
    index b@(m,_) i
        | inRange b i = fromIntegral (i - m)
        | otherwise   = indexError b i "Int64"
    inRange (m,n) i   = m <= i && i <= n
\end{code}
