{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

#include "MachDeps.h"
#include "WordSize.h"

module GHC.Internal.Bignum.Natural
    ( Natural(..)
    , naturalCheck#
    , naturalCheck

      -- * Useful constants
    , naturalZero
    , naturalOne

      -- * Predicates
    , naturalIsZero
    , naturalIsOne
    , naturalIsPowerOf2#

      -- * Conversion with...
      -- ** 'BigNat'
    , naturalFromBigNat#
    , naturalToBigNat#
      -- ** 'Word'
    , naturalFromWord#
    , naturalFromWord2#
    , naturalFromWord
    , naturalToWord#
    , naturalToWord
    , naturalToWordClamp#
    , naturalToWordClamp
    , naturalToWordMaybe#
      -- ** Limbs
    , naturalFromWordList
    , naturalToMutableByteArray#
    , naturalFromByteArray#
      -- ** Floating point
    , naturalEncodeDouble#
    , naturalEncodeFloat#
      -- ** 'Addr#'
    , naturalToAddr#
    , naturalToAddr
    , naturalFromAddr#
    , naturalFromAddr

      -- * Comparison
    , naturalEq#
    , naturalEq
    , naturalNe#
    , naturalNe
    , naturalGe#
    , naturalGe
    , naturalLe#
    , naturalLe
    , naturalGt#
    , naturalGt
    , naturalLt#
    , naturalLt
    , naturalCompare

      -- * Bit operations
    , naturalPopCount#
    , naturalPopCount
    , naturalShiftR#
    , naturalShiftR
    , naturalShiftL#
    , naturalShiftL
    , naturalAnd
    , naturalAndNot
    , naturalOr
    , naturalXor
    , naturalTestBit#
    , naturalTestBit
    , naturalBit#
    , naturalBit
    , naturalSetBit#
    , naturalSetBit
    , naturalClearBit#
    , naturalClearBit
    , naturalComplementBit#
    , naturalComplementBit

      -- * Arithmetic
    , naturalAdd
    , naturalSub
    , naturalSubThrow
    , naturalSubUnsafe
    , naturalMul
    , naturalSqr
    , naturalSignum
    , naturalNegate
    , naturalQuotRem#
    , naturalQuotRem
    , naturalQuot
    , naturalRem
    , naturalGcd
    , naturalLcm
    , naturalLog2#
    , naturalLog2
    , naturalLogBaseWord#
    , naturalLogBaseWord
    , naturalLogBase#
    , naturalLogBase
    , naturalPowMod

      -- * Miscellaneous
    , naturalSizeInBase#
    ) where

import GHC.Prim
import GHC.Types
import GHC.Classes

import GHC.Internal.Bignum.BigNat
import GHC.Internal.Bignum.Primitives

default ()

-- | Natural number
--
-- Invariant: numbers <= WORD_MAXBOUND use the `NS` constructor
data Natural
   = NS !Word#
   | NB !BigNat#

instance Eq Natural where
   (==) = naturalEq
   (/=) = naturalNe

instance Ord Natural where
   compare = naturalCompare
   (>)     = naturalGt
   (>=)    = naturalGe
   (<)     = naturalLt
   (<=)    = naturalLe


-- | Check Natural invariants
naturalCheck# :: Natural -> Bool#
naturalCheck# (NS _)  = 1#
naturalCheck# (NB bn) = bigNatCheck# bn &&# bigNatSize# bn ># 1#

-- | Check Natural invariants
naturalCheck :: Natural -> Bool
naturalCheck !n = isTrue# (naturalCheck# n)

-- | Zero Natural
naturalZero :: Natural
naturalZero = NS 0##

-- | One Natural
naturalOne :: Natural
naturalOne = NS 1##

-- | Test Zero Natural
naturalIsZero :: Natural -> Bool
naturalIsZero (NS 0##) = True
naturalIsZero _        = False

-- | Test One Natural
naturalIsOne :: Natural -> Bool
naturalIsOne (NS 1##) = True
naturalIsOne _        = False

-- | Indicate if the value is a power of two and which one
naturalIsPowerOf2# :: Natural -> (# (# #) | Word# #)
naturalIsPowerOf2# (NS w) = wordIsPowerOf2# w
naturalIsPowerOf2# (NB w) = bigNatIsPowerOf2# w

-- | Create a Natural from a BigNat# (respect the invariants)
naturalFromBigNat# :: BigNat# -> Natural
{-# NOINLINE naturalFromBigNat# #-}
naturalFromBigNat# x = case bigNatSize# x of
   0# -> naturalZero
   1# -> NS (bigNatIndex# x 0#)
   _  -> NB x

-- | Convert a Natural into a BigNat#
naturalToBigNat# :: Natural -> BigNat#
{-# NOINLINE naturalToBigNat# #-}
naturalToBigNat# (NS w)  = bigNatFromWord# w
naturalToBigNat# (NB bn) = bn

-- | Create a Natural from a Word#
naturalFromWord# :: Word# -> Natural
naturalFromWord# x = NS x

-- | Convert two Word# (most-significant first) into a Natural
naturalFromWord2# :: Word# -> Word# -> Natural
naturalFromWord2# 0## 0## = naturalZero
naturalFromWord2# 0## l   = NS l
naturalFromWord2# h   l   = NB (bigNatFromWord2# h l)

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# x) = NS x

-- | Create a Natural from a list of Word
naturalFromWordList :: [Word] -> Natural
naturalFromWordList xs = naturalFromBigNat# (bigNatFromWordList xs)

-- | Convert the lower bits of a Natural into a Word#
naturalToWord# :: Natural -> Word#
{-# NOINLINE naturalToWord# #-}
naturalToWord# (NS x) = x
naturalToWord# (NB b) = bigNatIndex# b 0#

-- | Convert the lower bits of a Natural into a Word
naturalToWord :: Natural -> Word
naturalToWord !n = W# (naturalToWord# n)

-- | Convert a Natural into a Word# clamping to (maxBound :: Word#).
naturalToWordClamp# :: Natural -> Word#
naturalToWordClamp# (NS x) = x
naturalToWordClamp# (NB _) = WORD_MAXBOUND##

-- | Convert a Natural into a Word# clamping to (maxBound :: Word).
naturalToWordClamp :: Natural -> Word
naturalToWordClamp !n = W# (naturalToWordClamp# n)

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns '(##)' if value doesn't fit in 'Word'.
naturalToWordMaybe# :: Natural -> (# (# #) | Word# #)
naturalToWordMaybe# (NS w) = (#       | w #)
naturalToWordMaybe# _      = (# (# #) |   #)

-- | Encode (# Natural mantissa, Int# exponent #) into a Double#
naturalEncodeDouble# :: Natural -> Int# -> Double#
naturalEncodeDouble# (NS w) 0# = word2Double# w
naturalEncodeDouble# (NS w) e  = wordEncodeDouble# w e
naturalEncodeDouble# (NB b) e  = bigNatEncodeDouble# b e

-- | Encode (# Natural mantissa, Int# exponent #) into a Float#
--
-- TODO: Not sure if it's worth to write 'Float' optimized versions here
naturalEncodeFloat# :: Natural -> Int# -> Float#
naturalEncodeFloat# !m e  = double2Float# (naturalEncodeDouble# m e)

-- | Equality test for Natural
naturalEq# :: Natural -> Natural -> Bool#
naturalEq# (NS x) (NS y) = x `eqWord#` y
naturalEq# (NB x) (NB y) = bigNatEq# x y
naturalEq# _      _      = 0#

-- | Equality test for Natural
naturalEq :: Natural -> Natural -> Bool
naturalEq !x !y = isTrue# (naturalEq# x y)

-- | Inequality test for Natural
naturalNe# :: Natural -> Natural -> Bool#
naturalNe# (NS x) (NS y) = x `neWord#` y
naturalNe# (NB x) (NB y) = bigNatNe# x y
naturalNe# _      _      = 1#

-- | Inequality test for Natural
naturalNe :: Natural -> Natural -> Bool
naturalNe !x !y = isTrue# (naturalNe# x y)

-- | Greater or equal test for Natural
naturalGe# :: Natural -> Natural -> Bool#
naturalGe# (NS x) (NS y) = x `geWord#` y
naturalGe# (NS _) (NB _) = 0#
naturalGe# (NB _) (NS _) = 1#
naturalGe# (NB x) (NB y) = bigNatGe# x y

-- | Greater or equal test for Natural
naturalGe :: Natural -> Natural -> Bool
naturalGe !x !y = isTrue# (naturalGe# x y)

-- | Lower or equal test for Natural
naturalLe# :: Natural -> Natural -> Bool#
naturalLe# (NS x) (NS y) = x `leWord#` y
naturalLe# (NS _) (NB _) = 1#
naturalLe# (NB _) (NS _) = 0#
naturalLe# (NB x) (NB y) = bigNatLe# x y

-- | Lower or equal test for Natural
naturalLe :: Natural -> Natural -> Bool
naturalLe !x !y = isTrue# (naturalLe# x y)


-- | Greater test for Natural
naturalGt# :: Natural -> Natural -> Bool#
naturalGt# (NS x) (NS y) = x `gtWord#` y
naturalGt# (NS _) (NB _) = 0#
naturalGt# (NB _) (NS _) = 1#
naturalGt# (NB x) (NB y) = bigNatGt# x y

-- | Greater test for Natural
naturalGt :: Natural -> Natural -> Bool
naturalGt !x !y = isTrue# (naturalGt# x y)

-- | Lower test for Natural
naturalLt# :: Natural -> Natural -> Bool#
naturalLt# (NS x) (NS y) = x `ltWord#` y
naturalLt# (NS _) (NB _) = 1#
naturalLt# (NB _) (NS _) = 0#
naturalLt# (NB x) (NB y) = bigNatLt# x y

-- | Lower test for Natural
naturalLt :: Natural -> Natural -> Bool
naturalLt !x !y = isTrue# (naturalLt# x y)

-- | Compare two Natural
naturalCompare :: Natural -> Natural -> Ordering
naturalCompare (NS x) (NS y) = cmpW# x y
naturalCompare (NB x) (NB y) = bigNatCompare x y
naturalCompare (NS _) (NB _) = LT
naturalCompare (NB _) (NS _) = GT

-- | PopCount for Natural
naturalPopCount# :: Natural -> Word#
{-# NOINLINE naturalPopCount# #-}
naturalPopCount# (NS x) = popCnt# x
naturalPopCount# (NB x) = bigNatPopCount# x

-- | PopCount for Natural
naturalPopCount :: Natural -> Word
naturalPopCount (NS x) = W# (popCnt# x)
naturalPopCount (NB x) = bigNatPopCount x

-- | Right shift for Natural
naturalShiftR# :: Natural -> Word# -> Natural
{-# NOINLINE naturalShiftR# #-}
naturalShiftR# (NS x) n = NS (x `shiftRW#` n)
naturalShiftR# (NB x) n = naturalFromBigNat# (x `bigNatShiftR#` n)

-- | Right shift for Natural
naturalShiftR :: Natural -> Word -> Natural
naturalShiftR x (W# n) = naturalShiftR# x n

-- | Left shift
naturalShiftL# :: Natural -> Word# -> Natural
{-# NOINLINE naturalShiftL# #-}
naturalShiftL# v@(NS x) n
   | 0## <- x                     = v
   | isTrue# (clz# x `geWord#` n) = NS (x `uncheckedShiftL#` word2Int# n)
   | True                         = NB (bigNatFromWord# x `bigNatShiftL#` n)
naturalShiftL# (NB x) n = NB (x `bigNatShiftL#` n)

-- | Left shift
naturalShiftL :: Natural -> Word -> Natural
naturalShiftL !x (W# n) = naturalShiftL# x n

-- | Add two naturals
naturalAdd :: Natural -> Natural -> Natural
{-# NOINLINE naturalAdd #-}
naturalAdd (NS x) (NB y) = NB (bigNatAddWord# y x)
naturalAdd (NB x) (NS y) = NB (bigNatAddWord# x y)
naturalAdd (NB x) (NB y) = NB (bigNatAdd x y)
naturalAdd (NS x) (NS y) =
   case addWordC# x y of
      (# l,0# #) -> NS l
      (# l,c  #) -> NB (bigNatFromWord2# (int2Word# c) l)

-- | Sub two naturals
naturalSub :: Natural -> Natural -> (# (# #) | Natural #)
{-# NOINLINE naturalSub #-}
naturalSub (NS _) (NB _) = (# (# #) | #)
naturalSub (NB x) (NS y) = (# | naturalFromBigNat# (bigNatSubWordUnsafe# x y) #)
naturalSub (NS x) (NS y) =
   case subWordC# x y of
      (# l,0# #) -> (#       | NS l #)
      (# _,_  #) -> (# (# #) |      #)
naturalSub (NB x) (NB y) =
   case bigNatSub x y of
      (# (# #) |    #) -> (# (# #) | #)
      (#       | z  #) -> (#       | naturalFromBigNat# z #)

-- | Sub two naturals
--
-- Throw an Underflow exception if x < y
naturalSubThrow :: Natural -> Natural -> Natural
{-# NOINLINE naturalSubThrow #-}
naturalSubThrow (NS _) (NB _) = raiseUnderflow
naturalSubThrow (NB x) (NS y) = naturalFromBigNat# (bigNatSubWordUnsafe# x y)
naturalSubThrow (NS x) (NS y) =
   case subWordC# x y of
      (# l,0# #) -> NS l
      (# _,_  #) -> raiseUnderflow
naturalSubThrow (NB x) (NB y) =
   case bigNatSub x y of
      (# (# #) |   #) -> raiseUnderflow
      (#       | z #) -> naturalFromBigNat# z

-- | Sub two naturals
--
-- Unsafe: don't check that x >= y
-- Undefined results if it happens
naturalSubUnsafe :: Natural -> Natural -> Natural
{-# NOINLINE naturalSubUnsafe #-}
naturalSubUnsafe (NS x) (NS y) = NS (minusWord# x y)
naturalSubUnsafe (NS _) (NB _) = naturalZero
naturalSubUnsafe (NB x) (NS y) = naturalFromBigNat# (bigNatSubWordUnsafe# x y)
naturalSubUnsafe (NB x) (NB y) =
   case bigNatSub x y of
      (# (# #) |   #) -> naturalZero
      (#       | z #) -> naturalFromBigNat# z

-- | Multiplication
naturalMul :: Natural -> Natural -> Natural
{-# NOINLINE naturalMul #-}
naturalMul a b = case a of
   NS 0## -> NS 0##
   NS 1## -> b
   NS x   -> case b of
               NS 0## -> NS 0##
               NS 1## -> a
               NS y   -> case timesWord2# x y of
                           (# h,l #) -> naturalFromWord2# h l
               NB y   -> NB (bigNatMulWord# y x)
   NB x   -> case b of
               NS 0## -> NS 0##
               NS 1## -> a
               NS y   -> NB (bigNatMulWord# x y)
               NB y   -> NB (bigNatMul x y)

-- | Square a Natural
naturalSqr :: Natural -> Natural
naturalSqr !a = naturalMul a a

-- | Signum for Natural
naturalSignum :: Natural -> Natural
naturalSignum (NS 0##) = NS 0##
naturalSignum _        = NS 1##

-- | Negate for Natural
naturalNegate :: Natural -> Natural
naturalNegate (NS 0##) = NS 0##
naturalNegate _        = raiseUnderflow

-- | Return division quotient and remainder
--
-- Division by zero is handled by BigNat
naturalQuotRem# :: Natural -> Natural -> (# Natural, Natural #)
{-# NOINLINE naturalQuotRem# #-}
naturalQuotRem# (NS n) (NS d) = case quotRemWord# n d of
                                 (# q, r #) -> (# NS q, NS r #)
naturalQuotRem# (NB n) (NS d) = case bigNatQuotRemWord# n d of
                                 (# q, r #) -> (# naturalFromBigNat# q, NS r #)
naturalQuotRem# (NS n) (NB d) = case bigNatQuotRem# (bigNatFromWord# n) d of
                                 (# q, r #) -> (# naturalFromBigNat# q, naturalFromBigNat# r #)
naturalQuotRem# (NB n) (NB d) = case bigNatQuotRem# n d of
                                 (# q, r #) -> (# naturalFromBigNat# q, naturalFromBigNat# r #)

-- | Return division quotient and remainder
naturalQuotRem :: Natural -> Natural -> (Natural, Natural)
naturalQuotRem !n !d = case naturalQuotRem# n d of
   (# q, r #) -> (q,r)

-- | Return division quotient
naturalQuot :: Natural -> Natural -> Natural
{-# NOINLINE naturalQuot #-}
naturalQuot (NS n) (NS d) = case quotWord# n d of
                             q -> NS q
naturalQuot (NB n) (NS d) = case bigNatQuotWord# n d of
                             q -> naturalFromBigNat# q
naturalQuot (NS n) (NB d) = case bigNatQuot (bigNatFromWord# n) d of
                             q -> naturalFromBigNat# q
naturalQuot (NB n) (NB d) = case bigNatQuot n d of
                             q -> naturalFromBigNat# q

-- | Return division remainder
naturalRem :: Natural -> Natural -> Natural
{-# NOINLINE naturalRem #-}
naturalRem (NS n) (NS d) = case remWord# n d of
                             r -> NS r
naturalRem (NB n) (NS d) = case bigNatRemWord# n d of
                             r -> NS r
naturalRem (NS n) (NB d) = case bigNatRem (bigNatFromWord# n) d of
                             r -> naturalFromBigNat# r
naturalRem (NB n) (NB d) = case bigNatRem n d of
                             r -> naturalFromBigNat# r

naturalAnd :: Natural -> Natural -> Natural
{-# NOINLINE naturalAnd #-}
naturalAnd (NS n) (NS m) = NS (n `and#` m)
naturalAnd (NS n) (NB m) = NS (n `and#` bigNatToWord# m)
naturalAnd (NB n) (NS m) = NS (bigNatToWord# n `and#` m)
naturalAnd (NB n) (NB m) = naturalFromBigNat# (bigNatAnd n m)

naturalAndNot :: Natural -> Natural -> Natural
{-# NOINLINE naturalAndNot #-}
naturalAndNot (NS n) (NS m) = NS (n `and#` not# m)
naturalAndNot (NS n) (NB m) = NS (n `and#` not# (bigNatToWord# m))
naturalAndNot (NB n) (NS m) = NS (bigNatToWord# n `and#` not# m)
naturalAndNot (NB n) (NB m) = naturalFromBigNat# (bigNatAndNot n m)

naturalOr :: Natural -> Natural -> Natural
{-# NOINLINE naturalOr #-}
naturalOr (NS n) (NS m) = NS (n `or#` m)
naturalOr (NS n) (NB m) = NB (bigNatOrWord# m n)
naturalOr (NB n) (NS m) = NB (bigNatOrWord# n m)
naturalOr (NB n) (NB m) = NB (bigNatOr n m)

naturalXor :: Natural -> Natural -> Natural
{-# NOINLINE naturalXor #-}
naturalXor (NS n) (NS m) = NS (n `xor#` m)
naturalXor (NS n) (NB m) = NB (bigNatXorWord# m n)
naturalXor (NB n) (NS m) = NB (bigNatXorWord# n m)
naturalXor (NB n) (NB m) = naturalFromBigNat# (bigNatXor n m)

naturalTestBit# :: Natural -> Word# -> Bool#
{-# NOINLINE naturalTestBit# #-}
naturalTestBit# (NS w) i  = (i `ltWord#` WORD_SIZE_IN_BITS##) &&#
                            ((w `and#` (1## `uncheckedShiftL#` word2Int# i)) `neWord#` 0##)
naturalTestBit# (NB bn) i = bigNatTestBit# bn i

naturalTestBit :: Natural -> Word -> Bool
naturalTestBit !n (W# i) = isTrue# (naturalTestBit# n i)

naturalBit# :: Word# -> Natural
{-# NOINLINE naturalBit# #-}
naturalBit# i
  | isTrue# (i `ltWord#` WORD_SIZE_IN_BITS##) = NS (1## `uncheckedShiftL#` word2Int# i)
  | True                                      = NB (bigNatBit# i)

naturalBit :: Word -> Natural
naturalBit (W# i) = naturalBit# i

-- | @since 1.3
naturalSetBit# :: Natural -> Word# -> Natural
naturalSetBit# (NS n) i
  | isTrue# (i `ltWord#` WORD_SIZE_IN_BITS##) = NS (n `or#` (1## `uncheckedShiftL#` word2Int# i))
  | True                                      = NB (bigNatSetBit# (bigNatFromWord# n) i)
naturalSetBit# (NB n) i                       = NB (bigNatSetBit# n i)

-- | @since 1.3
naturalSetBit :: Natural -> Word -> Natural
naturalSetBit !n (W# i) = naturalSetBit# n i

-- | @since 1.3
naturalClearBit# :: Natural -> Word# -> Natural
naturalClearBit# x@(NS n) i
  | isTrue# (i `ltWord#` WORD_SIZE_IN_BITS##) = NS (n `and#` not# (1## `uncheckedShiftL#` word2Int# i))
  | True                                      = x
naturalClearBit# (NB n) i                     = naturalFromBigNat# (bigNatClearBit# n i)

-- | @since 1.3
naturalClearBit :: Natural -> Word -> Natural
naturalClearBit !n (W# i) = naturalClearBit# n i

-- | @since 1.3
naturalComplementBit# :: Natural -> Word# -> Natural
naturalComplementBit# (NS n) i
  | isTrue# (i `ltWord#` WORD_SIZE_IN_BITS##) = NS (n `xor#` (1## `uncheckedShiftL#` word2Int# i))
  | True                                      = NB (bigNatSetBit# (bigNatFromWord# n) i)
naturalComplementBit# (NB n) i                = naturalFromBigNat# (bigNatComplementBit# n i)

-- | @since 1.3
naturalComplementBit :: Natural -> Word -> Natural
naturalComplementBit !n (W# i) = naturalComplementBit# n i

-- | Compute greatest common divisor.
naturalGcd :: Natural -> Natural -> Natural
{-# NOINLINE naturalGcd #-}
naturalGcd (NS 0##) !y       = y
naturalGcd x        (NS 0##) = x
naturalGcd (NS 1##) _        = NS 1##
naturalGcd _        (NS 1##) = NS 1##
naturalGcd (NB x)   (NB y)   = naturalFromBigNat# (bigNatGcd x y)
naturalGcd (NB x)   (NS y)   = NS (bigNatGcdWord# x y)
naturalGcd (NS x)   (NB y)   = NS (bigNatGcdWord# y x)
naturalGcd (NS x)   (NS y)   = NS (gcdWord# x y)

-- | Compute least common multiple.
naturalLcm :: Natural -> Natural -> Natural
{-# NOINLINE naturalLcm #-}
naturalLcm (NS 0##) !_       = NS 0##
naturalLcm _        (NS 0##) = NS 0##
naturalLcm (NS 1##) y        = y
naturalLcm x        (NS 1##) = x
naturalLcm (NS a  ) (NS b  ) = naturalFromBigNat# (bigNatLcmWordWord# a b)
naturalLcm (NB a  ) (NS b  ) = naturalFromBigNat# (bigNatLcmWord# a b)
naturalLcm (NS a  ) (NB b  ) = naturalFromBigNat# (bigNatLcmWord# b a)
naturalLcm (NB a  ) (NB b  ) = naturalFromBigNat# (bigNatLcm a b)

-- | Base 2 logarithm
naturalLog2# :: Natural -> Word#
{-# NOINLINE naturalLog2# #-}
naturalLog2# (NS w) = wordLog2# w
naturalLog2# (NB b) = bigNatLog2# b

-- | Base 2 logarithm
naturalLog2 :: Natural -> Word
naturalLog2 !n = W# (naturalLog2# n)

-- | Logarithm for an arbitrary base
naturalLogBaseWord# :: Word# -> Natural -> Word#
{-# NOINLINE naturalLogBaseWord# #-}
naturalLogBaseWord# base (NS a) = wordLogBase# base a
naturalLogBaseWord# base (NB a) = bigNatLogBaseWord# base a

-- | Logarithm for an arbitrary base
naturalLogBaseWord :: Word -> Natural -> Word
naturalLogBaseWord (W# base) !a = W# (naturalLogBaseWord# base a)

-- | Logarithm for an arbitrary base
naturalLogBase# :: Natural -> Natural -> Word#
{-# NOINLINE naturalLogBase# #-}
naturalLogBase# (NS base) !a     = naturalLogBaseWord# base a
naturalLogBase# (NB _   ) (NS _) = 0##
naturalLogBase# (NB base) (NB a) = bigNatLogBase# base a

-- | Logarithm for an arbitrary base
naturalLogBase :: Natural -> Natural -> Word
naturalLogBase !base !a = W# (naturalLogBase# base a)

-- | \"@'naturalPowMod' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
naturalPowMod :: Natural -> Natural -> Natural -> Natural
{-# NOINLINE naturalPowMod #-}
naturalPowMod !_         !_       (NS 0##) = raiseDivZero
naturalPowMod _          _        (NS 1##) = NS 0##
naturalPowMod _          (NS 0##) _        = NS 1##
naturalPowMod (NS 0##)   _        _        = NS 0##
naturalPowMod (NS 1##)   _        _        = NS 1##
naturalPowMod (NS b)    (NS e)   (NS m)    = NS (powModWord# b e m)
naturalPowMod b         e        (NS m)    = NS (bigNatPowModWord#
                                                   (naturalToBigNat# b)
                                                   (naturalToBigNat# e)
                                                    m)
naturalPowMod b         e        (NB m)    = naturalFromBigNat#
                                                (bigNatPowMod (naturalToBigNat# b)
                                                              (naturalToBigNat# e)
                                                              m)

-- | Compute the number of digits of the Natural in the given base.
--
-- `base` must be > 1
naturalSizeInBase# :: Word# -> Natural -> Word#
{-# NOINLINE naturalSizeInBase# #-}
naturalSizeInBase# base (NS w) = wordSizeInBase# base w
naturalSizeInBase# base (NB n) = bigNatSizeInBase# base n

-- | Write a 'Natural' to @/addr/@ in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
naturalToAddr# :: Natural -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
naturalToAddr# (NS i) = wordToAddr# i
naturalToAddr# (NB n) = bigNatToAddr# n

-- | Write a 'Natural' to @/addr/@ in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
naturalToAddr :: Natural -> Addr# -> Bool# -> IO Word
naturalToAddr a addr e = IO \s -> case naturalToAddr# a addr e s of
   (# s', w #) -> (# s', W# w #)


-- | Read a Natural in base-256 representation from an Addr#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
naturalFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, Natural #)
naturalFromAddr# sz addr e s =
   case bigNatFromAddr# sz addr e s of
      (# s', n #) -> (# s', naturalFromBigNat# n #)

-- | Read a Natural in base-256 representation from an Addr#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
naturalFromAddr :: Word# -> Addr# -> Bool# -> IO Natural
naturalFromAddr sz addr e = IO (naturalFromAddr# sz addr e)


-- | Write a Natural in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
naturalToMutableByteArray# :: Natural -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
naturalToMutableByteArray# (NS w) = wordToMutableByteArray# w
naturalToMutableByteArray# (NB a) = bigNatToMutableByteArray# a

-- | Read a Natural in base-256 representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
naturalFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> State# s -> (# State# s, Natural #)
naturalFromByteArray# sz ba off e s = case bigNatFromByteArray# sz ba off e s of
   (# s', a #) -> (# s', naturalFromBigNat# a #)



-- See Note [Optimising conversions between numeric types]
-- in GHC.Internal.Bignum.Integer
{-# RULES
"Word# -> Natural -> Word#"
  forall x. naturalToWord# (NS x) = x

"BigNat# -> Natural -> BigNat#"
  forall x. naturalToBigNat# (naturalFromBigNat# x) = x
#-}
