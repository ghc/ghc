{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}

-- |
-- Module      :  GHC.Num.Integer
-- Copyright   :  (c) Sylvain Henry 2019,
--                (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  sylvain@haskus.fr
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.

module GHC.Num.Integer where

#include "MachDeps.h"
#include "WordSize.h"

import GHC.Prim
import GHC.Types
import GHC.Classes
import GHC.Magic
import GHC.Num.Primitives
import GHC.Num.BigNat
import GHC.Num.Natural

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

default ()

-- | Arbitrary precision integers. In contrast with fixed-size integral types
-- such as 'Int', the 'Integer' type represents the entire infinite range of
-- integers.
--
-- Integers are stored in a kind of sign-magnitude form, hence do not expect
-- two's complement form when using bit operations.
--
-- If the value is small (fit into an 'Int'), 'IS' constructor is used.
-- Otherwise 'IP' and 'IN' constructors are used to store a 'BigNat'
-- representing respectively the positive or the negative value magnitude.
--
-- Invariant: 'IP' and 'IN' are used iff value doesn't fit in 'IS'
data Integer
   = IS !Int#   -- ^ iff value in @[minBound::'Int', maxBound::'Int']@ range
   | IP !BigNat -- ^ iff value in @]maxBound::'Int', +inf[@ range
   | IN !BigNat -- ^ iff value in @]-inf, minBound::'Int'[@ range


-- | Check Integer invariants
integerCheck# :: Integer -> Bool#
integerCheck# (IS  _) = 1#
integerCheck# (IP bn) = bigNatCheck# bn &&# (bn `bigNatGtWord#` INT_MAXBOUND##)
integerCheck# (IN bn) = bigNatCheck# bn &&# (bn `bigNatGtWord#` ABS_INT_MINBOUND##)

-- | Check Integer invariants
integerCheck :: Integer -> Bool
integerCheck i = isTrue# (integerCheck# i)

-- | Integer Zero
integerZero :: Integer
integerZero = IS 0#

-- | Integer One
integerOne :: Integer
integerOne = IS 1#

---------------------------------------------------------------------
-- Conversions
---------------------------------------------------------------------

-- | Create a positive Integer from a BigNat
integerFromBigNat :: BigNat -> Integer
integerFromBigNat !bn
   | bigNatIsZero bn
   = integerZero

   | isTrue# (bn `bigNatLeWord#` INT_MAXBOUND##)
   = IS (word2Int# (bigNatIndex# bn 0#))

   | True
   = IP bn

-- | Create a negative Integer from a BigNat
integerFromBigNatNeg :: BigNat -> Integer
integerFromBigNatNeg !bn
   | bigNatIsZero bn
   = integerZero

   | 1# <- bigNatSize# bn
   , i <- negateInt# (word2Int# (bigNatIndex# bn 0#))
   , isTrue# (i <=# 0#)
   = IS i

   | True
   = IN bn

-- | Create an Integer from a sign-bit and a BigNat
integerFromBigNatSign :: Int# -> BigNat -> Integer
integerFromBigNatSign !sign !bn
   | 0# <- sign
   = integerFromBigNat bn

   | True
   = integerFromBigNatNeg bn

-- | Convert an Integer into a BigNat.
--
-- Return 0 for negative Integers.
integerToBigNatClamp :: Integer -> BigNat
integerToBigNatClamp (IP x) = x
integerToBigNatClamp (IS x)
   | isTrue# (x >=# 0#)     = bigNatFromWord# (int2Word# x)
integerToBigNatClamp _      = bigNatZero void#

-- | Create an Integer from an Int#
integerFromInt# :: Int# -> Integer
integerFromInt# i = IS i

-- | Create an Integer from an Int
integerFromInt :: Int -> Integer
integerFromInt (I# i) = IS i

-- | Truncates 'Integer' to least-significant 'Int#'
integerToInt# :: Integer -> Int#
{-# NOINLINE integerToInt# #-}
integerToInt# (IS i) = i
integerToInt# (IP b) = word2Int# (bigNatToWord# b)
integerToInt# (IN b) = negateInt# (word2Int# (bigNatToWord# b))

-- | Truncates 'Integer' to least-significant 'Int#'
integerToInt :: Integer -> Int
integerToInt i = I# (integerToInt# i)

-- | Convert a Word# into an Integer
integerFromWord# :: Word# -> Integer
{-# NOINLINE integerFromWord# #-}
integerFromWord# w
   | i <- word2Int# w
   , isTrue# (i >=# 0#)
   = IS i

   | True
   = IP (bigNatFromWord# w)

-- | Convert a Word into an Integer
integerFromWord :: Word -> Integer
integerFromWord (W# w) = integerFromWord# w

-- | Create a negative Integer with the given Word magnitude
integerFromWordNeg# :: Word# -> Integer
integerFromWordNeg# w
  | isTrue# (w `leWord#` ABS_INT_MINBOUND##)
  = IS (negateInt# (word2Int# w))

  | True
  = IN (bigNatFromWord# w)

-- | Create an Integer from a sign and a Word magnitude
integerFromWordSign# :: Int# -> Word# -> Integer
integerFromWordSign# 0# w = integerFromWord# w
integerFromWordSign# _  w = integerFromWordNeg# w

-- | Truncate an Integer into a Word
integerToWord# :: Integer -> Word#
{-# NOINLINE integerToWord# #-}
integerToWord# (IS i)  = int2Word# i
integerToWord# (IP bn) = bigNatToWord# bn
integerToWord# (IN bn) = int2Word# (negateInt# (word2Int# (bigNatToWord# bn)))

-- | Truncate an Integer into a Word
integerToWord :: Integer -> Word
integerToWord !i = W# (integerToWord# i)

-- | Convert a Natural into an Integer
integerFromNatural :: Natural -> Integer
{-# NOINLINE integerFromNatural #-}
integerFromNatural (NS x) = integerFromWord# x
integerFromNatural (NB x) = integerFromBigNat x

-- | Convert a list of Word into an Integer
integerFromWordList :: Bool -> [Word] -> Integer
integerFromWordList True  ws = integerFromBigNatNeg (bigNatFromWordList ws)
integerFromWordList False ws = integerFromBigNat    (bigNatFromWordList ws)

-- | Convert a Integer into a Natural
--
-- Return 0 for negative Integers.
integerToNaturalClamp :: Integer -> Natural
{-# NOINLINE integerToNaturalClamp #-}
integerToNaturalClamp (IS x)
   | isTrue# (x <# 0#) = naturalZero
   | True              = naturalFromWord# (int2Word# x)
integerToNaturalClamp (IP x) = naturalFromBigNat x
integerToNaturalClamp (IN _) = naturalZero

-- | Convert a Integer into a Natural
--
-- Return absolute value
integerToNatural :: Integer -> Natural
{-# NOINLINE integerToNatural #-}
integerToNatural (IS x) = naturalFromWord# (wordFromAbsInt# x)
integerToNatural (IP x) = naturalFromBigNat x
integerToNatural (IN x) = naturalFromBigNat x

---------------------------------------------------------------------
-- Predicates
---------------------------------------------------------------------

-- | Negative predicate
integerIsNegative# :: Integer -> Bool#
integerIsNegative# (IS i#) = i# <# 0#
integerIsNegative# (IP _)  = 0#
integerIsNegative# (IN _)  = 1#

-- | Negative predicate
integerIsNegative :: Integer -> Bool
integerIsNegative !i = isTrue# (integerIsNegative# i)

-- | Zero predicate
integerIsZero :: Integer -> Bool
integerIsZero (IS 0#) = True
integerIsZero _       = False

-- | Not-equal predicate.
integerNe :: Integer -> Integer -> Bool
integerNe !x !y = isTrue# (integerNe# x y)

-- | Equal predicate.
integerEq :: Integer -> Integer -> Bool
integerEq !x !y = isTrue# (integerEq# x y)

-- | Lower-or-equal predicate.
integerLe :: Integer -> Integer -> Bool
integerLe !x !y = isTrue# (integerLe# x y)

-- | Lower predicate.
integerLt :: Integer -> Integer -> Bool
integerLt !x !y = isTrue# (integerLt# x y)

-- | Greater predicate.
integerGt :: Integer -> Integer -> Bool
integerGt !x !y = isTrue# (integerGt# x y)

-- | Greater-or-equal predicate.
integerGe :: Integer -> Integer -> Bool
integerGe !x !y = isTrue# (integerGe# x y)

-- | Equal predicate.
integerEq# :: Integer -> Integer -> Bool#
{-# NOINLINE integerEq# #-}
integerEq# (IS x) (IS y) = x ==# y
integerEq# (IN x) (IN y) = bigNatEq# x y
integerEq# (IP x) (IP y) = bigNatEq# x y
integerEq# _       _     = 0#

-- | Not-equal predicate.
integerNe# :: Integer -> Integer -> Bool#
{-# NOINLINE integerNe# #-}
integerNe# (IS x) (IS y) = x /=# y
integerNe# (IN x) (IN y) = bigNatNe# x y
integerNe# (IP x) (IP y) = bigNatNe# x y
integerNe# _       _     = 1#

-- | Greater predicate.
integerGt# :: Integer -> Integer -> Bool#
{-# NOINLINE integerGt# #-}
integerGt# (IS x) (IS y)                  = x ># y
integerGt# x y | GT <- integerCompare x y = 1#
integerGt# _ _                            = 0#

-- | Lower-or-equal predicate.
integerLe# :: Integer -> Integer -> Bool#
{-# NOINLINE integerLe# #-}
integerLe# (IS x) (IS y)                  = x <=# y
integerLe# x y | GT <- integerCompare x y = 0#
integerLe# _ _                            = 1#

-- | Lower predicate.
integerLt# :: Integer -> Integer -> Bool#
{-# NOINLINE integerLt# #-}
integerLt# (IS x) (IS y)                  = x <# y
integerLt# x y | LT <- integerCompare x y = 1#
integerLt# _ _                            = 0#

-- | Greater-or-equal predicate.
integerGe# :: Integer -> Integer -> Bool#
{-# NOINLINE integerGe# #-}
integerGe# (IS x) (IS y)                  = x >=# y
integerGe# x y | LT <- integerCompare x y = 0#
integerGe# _ _                            = 1#

instance Eq Integer where
   (==) = integerEq
   (/=) = integerNe

-- | Compare two Integer
integerCompare :: Integer -> Integer -> Ordering
{-# NOINLINE integerCompare #-}
integerCompare (IS x) (IS y) = compareInt# x y
integerCompare (IP x) (IP y) = bigNatCompare x y
integerCompare (IN x) (IN y) = bigNatCompare y x
integerCompare (IS _) (IP _) = LT
integerCompare (IS _) (IN _) = GT
integerCompare (IP _) (IS _) = GT
integerCompare (IN _) (IS _) = LT
integerCompare (IP _) (IN _) = GT
integerCompare (IN _) (IP _) = LT

instance Ord Integer where
   compare = integerCompare

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

-- | Subtract one 'Integer' from another.
integerSub :: Integer -> Integer -> Integer
{-# NOINLINE integerSub #-}
integerSub !x      (IS 0#) = x
integerSub (IS x#) (IS y#)
  = case subIntC# x# y# of
    (# z#, 0# #) -> IS z#
    (# 0#, _  #) -> IN (bigNatFromWord2# 1## 0##)
    (# z#, _  #)
      | isTrue# (z# ># 0#)
      -> IN (bigNatFromWord# ( (int2Word# (negateInt# z#))))
      | True
      -> IP (bigNatFromWord# ( (int2Word# z#)))
integerSub (IS x#) (IP y)
  | isTrue# (x# >=# 0#)
  = integerFromBigNatNeg (bigNatSubWordUnsafe# y (int2Word# x#))
  | True
  = IN (bigNatAddWord# y (int2Word# (negateInt# x#)))
integerSub (IS x#) (IN y)
  | isTrue# (x# >=# 0#)
  = IP (bigNatAddWord# y (int2Word# x#))
  | True
  = integerFromBigNat (bigNatSubWordUnsafe# y (int2Word# (negateInt# x#)))
integerSub (IP x) (IP y)
  = case bigNatCompare x y of
    LT -> integerFromBigNatNeg (bigNatSubUnsafe y x)
    EQ -> IS 0#
    GT -> integerFromBigNat (bigNatSubUnsafe x y)
integerSub (IP x) (IN y) = IP (bigNatAdd x y)
integerSub (IN x) (IP y) = IN (bigNatAdd x y)
integerSub (IN x) (IN y)
  = case bigNatCompare x y of
    LT -> integerFromBigNat (bigNatSubUnsafe y x)
    EQ -> IS 0#
    GT -> integerFromBigNatNeg (bigNatSubUnsafe x y)
integerSub (IP x) (IS y#)
  | isTrue# (y# >=# 0#)
  = integerFromBigNat (bigNatSubWordUnsafe# x (int2Word# y#))
  | True
  = IP (bigNatAddWord# x (int2Word# (negateInt# y#)))
integerSub (IN x) (IS y#)
  | isTrue# (y# >=# 0#)
  = IN (bigNatAddWord# x (int2Word# y#))
  | True
  = integerFromBigNatNeg (bigNatSubWordUnsafe# x (int2Word# (negateInt# y#)))

-- | Add two 'Integer's
integerAdd :: Integer -> Integer -> Integer
{-# NOINLINE integerAdd #-}
integerAdd !x      (IS 0#) = x
integerAdd (IS 0#) y       = y
integerAdd (IS x#) (IS y#)
  = case addIntC# x# y# of
    (# z#, 0# #) -> IS z#
    (# 0#, _  #) -> IN (bigNatFromWord2# 1## 0##) -- 2*minBound::Int
    (# z#, _  #)
      | isTrue# (z# ># 0#) -> IN (bigNatFromWord# ( (int2Word# (negateInt# z#))))
      | True               -> IP (bigNatFromWord# ( (int2Word# z#)))
integerAdd y@(IS _) x = integerAdd x y
integerAdd (IP x) (IP y) = IP (bigNatAdd x y)
integerAdd (IN x) (IN y) = IN (bigNatAdd x y)
integerAdd (IP x) (IS y#) -- edge-case: @(maxBound+1) + minBound == 0@
  | isTrue# (y# >=# 0#) = IP (bigNatAddWord# x (int2Word# y#))
  | True                = integerFromBigNat (bigNatSubWordUnsafe# x (int2Word#
                                                              (negateInt# y#)))
integerAdd (IN x) (IS y#) -- edge-case: @(minBound-1) + maxBound == -2@
  | isTrue# (y# >=# 0#) = integerFromBigNatNeg (bigNatSubWordUnsafe# x (int2Word# y#))
  | True                = IN (bigNatAddWord# x (int2Word# (negateInt# y#)))
integerAdd y@(IN _) x@(IP _) = integerAdd x y
integerAdd (IP x) (IN y)
    = case bigNatCompare x y of
      LT -> integerFromBigNatNeg (bigNatSubUnsafe y x)
      EQ -> IS 0#
      GT -> integerFromBigNat (bigNatSubUnsafe x y)

-- | Multiply two 'Integer's
integerMul :: Integer -> Integer -> Integer
{-# NOINLINE integerMul #-}
integerMul !_       (IS 0#)  = IS 0#
integerMul (IS 0#)  _        = IS 0#
integerMul x        (IS 1#)  = x
integerMul (IS 1#)  y        = y
integerMul x        (IS -1#) = integerNegate x
integerMul (IS -1#) y        = integerNegate y
#if __GLASGOW_HASKELL__ < 811
integerMul (IS x)   (IS y)   = case mulIntMayOflo# x y of
   0# -> IS (x *# y)
   _  -> case (# isTrue# (x >=# 0#), isTrue# (y >=# 0#) #) of
      (# False, False #) -> case timesWord2# (int2Word# (negateInt# x))
                                       (int2Word# (negateInt# y)) of
          (# 0##,l #) -> integerFromWord# l
          (# h  ,l #) -> IP (bigNatFromWord2# h l)

      (#  True, False #) -> case timesWord2# (int2Word# x)
                                       (int2Word# (negateInt# y)) of
          (# 0##,l #) -> integerFromWordNeg# l
          (# h  ,l #) -> IN (bigNatFromWord2# h l)

      (# False,  True #) -> case timesWord2# (int2Word# (negateInt# x))
                                       (int2Word# y) of
          (# 0##,l #) -> integerFromWordNeg# l
          (# h  ,l #) -> IN (bigNatFromWord2# h l)

      (#  True,  True #) -> case timesWord2# (int2Word# x)
                                       (int2Word# y) of
          (# 0##,l #) -> integerFromWord# l
          (# h  ,l #) -> IP (bigNatFromWord2# h l)
#else
integerMul (IS x)   (IS y)   = case timesInt2# x y of
   (# 0#, _h, l #) -> IS l
   (# _ ,  h, l #)
      | isTrue# (h >=# 0#)
      -> IP (bigNatFromWord2# (int2Word# h) (int2Word# l))
      | True
      -> let
          -- two's complement of a two-word negative Int:
          --   l' = complement l + 1
          --   h' = complement h + carry
          !(# l',c #) = addWordC# (not# (int2Word# l)) 1##
          !h'         = int2Word# c `plusWord#` not# (int2Word# h)
         in IN (bigNatFromWord2# h' l')
#endif
integerMul x@(IS _) y    = integerMul y x
integerMul (IP x) (IP y) = IP (bigNatMul x y)
integerMul (IP x) (IN y) = IN (bigNatMul x y)
integerMul (IP x) (IS y)
  | isTrue# (y >=# 0#)   = IP (bigNatMulWord# x (int2Word# y))
  | True                 = IN (bigNatMulWord# x (int2Word# (negateInt# y)))
integerMul (IN x) (IN y) = IP (bigNatMul x y)
integerMul (IN x) (IP y) = IN (bigNatMul x y)
integerMul (IN x) (IS y)
  | isTrue# (y >=# 0#)   = IN (bigNatMulWord# x (int2Word# y))
  | True                 = IP (bigNatMulWord# x (int2Word# (negateInt# y)))

-- | Negate 'Integer'.
--
-- One edge-case issue to take into account is that Int's range is not
-- symmetric around 0.  I.e. @minBound+maxBound = -1@
--
-- IP is used iff n > maxBound::Int
-- IN is used iff n < minBound::Int
integerNegate :: Integer -> Integer
{-# NOINLINE integerNegate #-}
integerNegate (IN b)             = IP b
integerNegate (IS INT_MINBOUND#) = IP (bigNatFromWord# ABS_INT_MINBOUND##)
integerNegate (IS i)             = IS (negateInt# i)
integerNegate (IP b)
  | isTrue# (bigNatEqWord# b ABS_INT_MINBOUND##) = IS INT_MINBOUND#
  | True                                         = IN b


-- | Compute absolute value of an 'Integer'
integerAbs :: Integer -> Integer
{-# NOINLINE integerAbs #-}
integerAbs   (IN i)     = IP i
integerAbs n@(IP _)     = n
integerAbs n@(IS i)
   | isTrue# (i >=# 0#) = n
   | INT_MINBOUND# <- i = IP (bigNatFromWord# ABS_INT_MINBOUND##)
   | True               = IS (negateInt# i)


-- | Return @-1@, @0@, and @1@ depending on whether argument is
-- negative, zero, or positive, respectively
integerSignum :: Integer -> Integer
{-# NOINLINE integerSignum #-}
integerSignum !j = IS (integerSignum# j)

-- | Return @-1#@, @0#@, and @1#@ depending on whether argument is
-- negative, zero, or positive, respectively
integerSignum# :: Integer -> Int#
{-# NOINLINE integerSignum# #-}
integerSignum# (IN _)  = -1#
integerSignum# (IS i#) = sgnI# i#
integerSignum# (IP _ ) =  1#

-- | Count number of set bits. For negative arguments returns
-- the negated population count of the absolute value.
integerPopCount# :: Integer -> Int#
{-# NOINLINE integerPopCount# #-}
integerPopCount# (IS i)
   | isTrue# (i >=# 0#)  = word2Int# (popCntI# i)
   | True                = negateInt# (word2Int# (popCntI# (negateInt# i)))
integerPopCount# (IP bn) = word2Int# (bigNatPopCount# bn)
integerPopCount# (IN bn) = negateInt# (word2Int# (bigNatPopCount# bn))

-- | Positive 'Integer' for which only /n/-th bit is set
integerBit# :: Word# -> Integer
{-# NOINLINE integerBit# #-}
integerBit# i
  | isTrue# (i `ltWord#` (WORD_SIZE_IN_BITS## `minusWord#` 1##))
  = IS (uncheckedIShiftL# 1# (word2Int# i))

  | True = IP (bigNatBit# i)

-- | 'Integer' for which only /n/-th bit is set
integerBit :: Word -> Integer
integerBit (W# i) = integerBit# i

-- | Test if /n/-th bit is set.
--
-- Fake 2's complement for negative values (might be slow)
integerTestBit# :: Integer -> Word# -> Bool#
{-# NOINLINE integerTestBit# #-}
integerTestBit# (IS x) i
   | isTrue# (i `ltWord#` WORD_SIZE_IN_BITS##)
   = testBitI# x i
   | True
   = x <# 0#
integerTestBit# (IP x) i = bigNatTestBit# x i
integerTestBit# (IN x) i
   | isTrue# (iw >=# n)
   = 1#
   -- if all the limbs j with j < iw are null, then we have to consider the
   -- carry of the 2's complement convertion. Otherwise we just have to return
   -- the inverse of the bit test
   | allZ iw = testBitW# (xi `minusWord#` 1##) ib ==# 0#
   | True    = testBitW# xi ib ==# 0#
   where
      !xi  = bigNatIndex# x iw
      !n   = bigNatSize# x
      !iw  = word2Int# (i `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !ib  = i `and#` WORD_SIZE_BITS_MASK##

      allZ 0# = True
      allZ j | isTrue# (bigNatIndex# x (j -# 1#) `eqWord#` 0##) = allZ (j -# 1#)
             | True                 = False

-- | Test if /n/-th bit is set. For negative Integers it tests the n-th bit of
-- the negated argument.
--
-- Fake 2's complement for negative values (might be slow)
integerTestBit :: Integer -> Word -> Bool
integerTestBit !i (W# n) = isTrue# (integerTestBit# i n)

-- | Shift-right operation
--
-- Fake 2's complement for negative values (might be slow)
integerShiftR# :: Integer -> Word# -> Integer
{-# NOINLINE integerShiftR# #-}
integerShiftR# !x      0## = x
integerShiftR# (IS i)  n   = IS (iShiftRA# i (word2Int# n))
  where
    iShiftRA# a b
      | isTrue# (b >=# WORD_SIZE_IN_BITS#) = (a <# 0#) *# (-1#)
      | True                               = a `uncheckedIShiftRA#` b
integerShiftR# (IP bn) n   = integerFromBigNat (bigNatShiftR# bn n)
integerShiftR# (IN bn) n   =
   case integerFromBigNatNeg (bigNatShiftRNeg# bn n) of
      IS 0# -> IS -1#
      r     -> r

-- | Shift-right operation
--
-- Fake 2's complement for negative values (might be slow)
integerShiftR :: Integer -> Word -> Integer
integerShiftR !x (W# w) = integerShiftR# x w

-- | Shift-left operation
integerShiftL# :: Integer -> Word# -> Integer
{-# NOINLINE integerShiftL# #-}
integerShiftL# !x      0## = x
integerShiftL# (IS 0#) _   = IS 0#
integerShiftL# (IS 1#) n   = integerBit# n
integerShiftL# (IS i)  n
  | isTrue# (i >=# 0#) = integerFromBigNat    (bigNatShiftL# (bigNatFromWord# (int2Word# i)) n)
  | True               = integerFromBigNatNeg (bigNatShiftL# (bigNatFromWord# (int2Word# (negateInt# i))) n)
integerShiftL# (IP bn) n   = IP (bigNatShiftL# bn n)
integerShiftL# (IN bn) n   = IN (bigNatShiftL# bn n)

-- | Shift-left operation
--
-- Remember that bits are stored in sign-magnitude form, hence the behavior of
-- negative Integers is different from negative Int's behavior.
integerShiftL :: Integer -> Word -> Integer
integerShiftL !x (W# w) = integerShiftL# x w

-- | Bitwise OR operation
--
-- Fake 2's complement for negative values (might be slow)
integerOr :: Integer -> Integer -> Integer
{-# NOINLINE integerOr #-}
integerOr a b = case a of
   IS  0# -> b
   IS -1# -> IS -1#
   IS  x  -> case b of
               IS  0# -> a
               IS -1# -> IS -1#
               IS  y  -> IS (orI# x y)
               IP  y
                  | isTrue# (x >=# 0#) -> integerFromBigNat (bigNatOrWord# y (int2Word# x))
                  | True               -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatAndNot -- use De Morgan's laws
                                                   (bigNatFromWord#
                                                      (int2Word# (negateInt# x) `minusWord#` 1##))
                                                   y)
                                                1##)
               IN y
                  | isTrue# (x >=# 0#) -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatAndNotWord# -- use De Morgan's laws
                                                   (bigNatSubWordUnsafe# y 1##)
                                                   (int2Word# x))
                                                1##)
                  | True               -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatAndWord#  -- use De Morgan's laws
                                                   (bigNatSubWordUnsafe# y 1##)
                                                   (int2Word# (negateInt# x) `minusWord#` 1##))
                                                1##)
   IP  x  -> case b of
               IS _ -> integerOr b a
               IP y -> integerFromBigNat (bigNatOr x y)
               IN y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatAndNot -- use De Morgan's laws
                              (bigNatSubWordUnsafe# y 1##)
                              x)
                           1##)
   IN  x  -> case b of
               IS _ -> integerOr b a
               IN y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatAnd  -- use De Morgan's laws
                              (bigNatSubWordUnsafe# x 1##)
                              (bigNatSubWordUnsafe# y 1##))
                           1##)
               IP y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatAndNot -- use De Morgan's laws
                              (bigNatSubWordUnsafe# x 1##)
                              y)
                           1##)


-- | Bitwise XOR operation
--
-- Fake 2's complement for negative values (might be slow)
integerXor :: Integer -> Integer -> Integer
{-# NOINLINE integerXor #-}
integerXor a b = case a of
   IS  0# -> b
   IS -1# -> integerComplement b
   IS x   -> case b of
               IS  0# -> a
               IS -1# -> integerComplement a
               IS y   -> IS (xorI# x y)
               IP y
                  | isTrue# (x >=# 0#) -> integerFromBigNat (bigNatXorWord# y (int2Word# x))
                  | True               -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatXorWord#
                                                   y
                                                   (int2Word# (negateInt# x) `minusWord#` 1##))
                                                1##)
               IN y
                  | isTrue# (x >=# 0#) -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatXorWord#
                                                   (bigNatSubWordUnsafe# y 1##)
                                                   (int2Word# x))
                                                1##)
                  | True               -> integerFromBigNat
                                             (bigNatXorWord# -- xor (not x) (not y) = xor x y
                                                (bigNatSubWordUnsafe# y 1##)
                                                (int2Word# (negateInt# x) `minusWord#` 1##))
   IP x   -> case b of
               IS _ -> integerXor b a
               IP y -> integerFromBigNat (bigNatXor x y)
               IN y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatXor
                              x
                              (bigNatSubWordUnsafe# y 1##))
                           1##)
   IN x   -> case b of
               IS _ -> integerXor b a
               IN y -> integerFromBigNat
                        (bigNatXor -- xor (not x) (not y) = xor x y
                           (bigNatSubWordUnsafe# x 1##)
                           (bigNatSubWordUnsafe# y 1##))
               IP y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatXor
                              y
                              (bigNatSubWordUnsafe# x 1##))
                           1##)



-- | Bitwise AND operation
--
-- Fake 2's complement for negative values (might be slow)
integerAnd :: Integer -> Integer -> Integer
{-# NOINLINE integerAnd #-}
integerAnd a b = case a of
   IS 0#  -> IS 0#
   IS -1# -> b
   IS x   -> case b of
               IS  0# -> IS 0#
               IS -1# -> a
               IS y   -> IS (andI# x y)
               IP y   -> integerFromBigNat (bigNatAndInt# y x)
               IN y
                  | isTrue# (x >=# 0#) -> integerFromWord# (int2Word# x `andNot#` (indexWordArray# y 0# `minusWord#` 1##))
                  | True               -> integerFromBigNatNeg
                                             (bigNatAddWord#
                                                (bigNatOrWord#  -- use De Morgan's laws
                                                   (bigNatSubWordUnsafe# y 1##)
                                                   (wordFromAbsInt# x `minusWord#` 1##))
                                                1##)
   IP x   -> case b of
               IS _ -> integerAnd b a
               IP y -> integerFromBigNat (bigNatAnd x y)
               IN y -> integerFromBigNat (bigNatAndNot x (bigNatSubWordUnsafe# y 1##))
   IN x   -> case b of
               IS _ -> integerAnd b a
               IN y -> integerFromBigNatNeg
                        (bigNatAddWord#
                           (bigNatOr  -- use De Morgan's laws
                              (bigNatSubWordUnsafe# x 1##)
                              (bigNatSubWordUnsafe# y 1##))
                           1##)
               IP y -> integerFromBigNat (bigNatAndNot y (bigNatSubWordUnsafe# x 1##))



-- | Binary complement of the
integerComplement :: Integer -> Integer
{-# NOINLINE integerComplement #-}
integerComplement (IS x) = IS (notI# x)
integerComplement (IP x) = IN (bigNatAddWord# x 1##)
integerComplement (IN x) = IP (bigNatSubWordUnsafe# x 1##)


-- | Simultaneous 'integerQuot' and 'integerRem'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
integerQuotRem# :: Integer -> Integer -> (# Integer, Integer #)
{-# NOINLINE integerQuotRem# #-}
integerQuotRem# !n      (IS 1#) = (# n, IS 0# #)
integerQuotRem# !n     (IS -1#) = let !q = integerNegate n in (# q, (IS 0#) #)
integerQuotRem# !_      (IS 0#) = (# divByZero, divByZero #)
integerQuotRem# (IS 0#) _       = (# IS 0#, IS 0# #)
integerQuotRem# (IS n#) (IS d#) = case quotRemInt# n# d# of
    (# q#, r# #) -> (# IS q#, IS r# #)
integerQuotRem# (IP n)  (IP d)  = case bigNatQuotRem# n d of
    (# q, r #) -> (# integerFromBigNat q, integerFromBigNat r #)
integerQuotRem# (IP n)  (IN d)  = case bigNatQuotRem# n d of
    (# q, r #) -> (# integerFromBigNatNeg q, integerFromBigNat r #)
integerQuotRem# (IN n)  (IN d)  = case bigNatQuotRem# n d of
    (# q, r #) -> (# integerFromBigNat q, integerFromBigNatNeg r #)
integerQuotRem# (IN n)  (IP d)  = case bigNatQuotRem# n d of
    (# q, r #) -> (# integerFromBigNatNeg q, integerFromBigNatNeg r #)
integerQuotRem# (IP n)  (IS d#)
  | isTrue# (d# >=# 0#) = case bigNatQuotRemWord# n (int2Word# d#) of
      (# q, r# #) -> (# integerFromBigNat q, integerFromWord# r# #)
  | True                = case bigNatQuotRemWord# n (int2Word# (negateInt# d#)) of
      (# q, r# #) -> (# integerFromBigNatNeg q, integerFromWord# r# #)
integerQuotRem# (IN n)  (IS d#)
  | isTrue# (d# >=# 0#) = case bigNatQuotRemWord# n (int2Word# d#) of
      (# q, r# #) -> (# integerFromBigNatNeg q, integerFromWordNeg# r# #)
  | True                = case bigNatQuotRemWord# n (int2Word# (negateInt# d#)) of
      (# q, r# #) -> (# integerFromBigNat q, integerFromWordNeg# r# #)
integerQuotRem# n@(IS _) (IN _) = (# IS 0#, n #) -- since @n < d@
integerQuotRem# n@(IS n#) (IP d) -- need to account for (IS minBound)
    | isTrue# (n# ># 0#)                                    = (# IS 0#, n #)
    | isTrue# (bigNatGtWord# d (int2Word# (negateInt# n#))) = (# IS 0#, n #)
    | True {- abs(n) == d -}                                = (# IS -1#, IS 0# #)

-- | Simultaneous 'integerQuot' and 'integerRem'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
integerQuotRem :: Integer -> Integer -> (Integer, Integer)
integerQuotRem !x !y = case integerQuotRem# x y of
   (# q, r #) -> (q, r)


integerQuot :: Integer -> Integer -> Integer
{-# NOINLINE integerQuot #-}
integerQuot !n      (IS 1#)  = n
integerQuot !n      (IS -1#) = integerNegate n
integerQuot !_      (IS 0#)  = divByZero
integerQuot (IS 0#) _        = IS 0#
integerQuot (IS n#) (IS d#)  = IS (quotInt# n# d#)
integerQuot (IP n)  (IS d#)
  | isTrue# (d# >=# 0#) = integerFromBigNat    (bigNatQuotWord# n (int2Word# d#))
  | True                = integerFromBigNatNeg (bigNatQuotWord# n
                                              (int2Word# (negateInt# d#)))
integerQuot (IN n)   (IS d#)
  | isTrue# (d# >=# 0#) = integerFromBigNatNeg (bigNatQuotWord# n (int2Word# d#))
  | True                = integerFromBigNat    (bigNatQuotWord# n
                                              (int2Word# (negateInt# d#)))
integerQuot (IP n) (IP d) = integerFromBigNat    (bigNatQuot n d)
integerQuot (IP n) (IN d) = integerFromBigNatNeg (bigNatQuot n d)
integerQuot (IN n) (IP d) = integerFromBigNatNeg (bigNatQuot n d)
integerQuot (IN n) (IN d) = integerFromBigNat    (bigNatQuot n d)
integerQuot n d = case integerQuotRem# n d of (# q, _ #) -> q

integerRem :: Integer -> Integer -> Integer
{-# NOINLINE integerRem #-}
integerRem !_       (IS 1#) = IS 0#
integerRem _       (IS -1#) = IS 0#
integerRem _        (IS 0#) = IS (remInt# 0# 0#)
integerRem (IS 0#) _        = IS 0#
integerRem (IS n#) (IS d#) = IS (remInt# n# d#)
integerRem (IP n)  (IS d#)
    = integerFromWord#    (bigNatRemWord# n (int2Word# (absI# d#)))
integerRem (IN n)  (IS d#)
    = integerFromWordNeg# (bigNatRemWord# n (int2Word# (absI# d#)))
integerRem (IP n)  (IP d)  = integerFromBigNat    (bigNatRem n d)
integerRem (IP n)  (IN d)  = integerFromBigNat    (bigNatRem n d)
integerRem (IN n)  (IP d)  = integerFromBigNatNeg (bigNatRem n d)
integerRem (IN n)  (IN d)  = integerFromBigNatNeg (bigNatRem n d)
integerRem n d = case integerQuotRem# n d of (# _, r #) -> r


-- | Simultaneous 'integerDiv' and 'integerMod'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
integerDivMod# :: Integer -> Integer -> (# Integer, Integer #)
{-# NOINLINE integerDivMod# #-}
integerDivMod# !n !d
  | isTrue# (integerSignum# r ==# negateInt# (integerSignum# d))
     = let !q' = integerAdd q (IS -1#) -- TODO: optimize
           !r' = integerAdd r d
       in (# q', r' #)
  | True = qr
  where
    !qr@(# q, r #) = integerQuotRem# n d

-- | Simultaneous 'integerDiv' and 'integerMod'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
integerDivMod :: Integer -> Integer -> (Integer, Integer)
integerDivMod !n !d = case integerDivMod# n d of
   (# q,r #) -> (q,r)


integerDiv :: Integer -> Integer -> Integer
{-# NOINLINE integerDiv #-}
integerDiv !n !d
   -- same-sign ops can be handled by more efficient 'integerQuot'
   | isTrue# (integerIsNegative# n ==# integerIsNegative# d) = integerQuot n d
   | True = case integerDivMod# n d of (# q, _ #) -> q


integerMod :: Integer -> Integer -> Integer
{-# NOINLINE integerMod #-}
integerMod !n !d
   -- same-sign ops can be handled by more efficient 'integerRem'
   | isTrue# (integerIsNegative# n ==# integerIsNegative# d) = integerRem n d
   | True = case integerDivMod# n d of (# _, r #) -> r

-- | Compute greatest common divisor.
integerGcd :: Integer -> Integer -> Integer
{-# NOINLINE integerGcd #-}
integerGcd (IS 0#)  !b       = integerAbs b
integerGcd a        (IS 0#)  = integerAbs a
integerGcd (IS 1#)  _        = IS 1#
integerGcd (IS -1#) _        = IS 1#
integerGcd _        (IS 1#)  = IS 1#
integerGcd _        (IS -1#) = IS 1#
integerGcd (IS a)   (IS b)   = integerFromWord# (gcdWord#
                                 (int2Word# (absI# a))
                                 (int2Word# (absI# b)))
integerGcd a@(IS _) b        = integerGcd b a
integerGcd (IN a)   b        = integerGcd (IP a) b
integerGcd (IP a)   (IP b)   = integerFromBigNat (bigNatGcd a b)
integerGcd (IP a)   (IN b)   = integerFromBigNat (bigNatGcd a b)
integerGcd (IP a)   (IS b)   = integerFromWord# (bigNatGcdWord# a (int2Word# (absI# b)))

-- | Compute least common multiple.
integerLcm :: Integer -> Integer -> Integer
{-# NOINLINE integerLcm #-}
integerLcm (IS 0#) !_  = IS 0#
integerLcm (IS 1#)  b  = integerAbs b
integerLcm (IS -1#) b  = integerAbs b
integerLcm _ (IS 0#)   = IS 0#
integerLcm a (IS 1#)   = integerAbs a
integerLcm a (IS -1#)  = integerAbs a
integerLcm a b         = (aa `integerQuot` (aa `integerGcd` ab)) `integerMul` ab
  where                   -- TODO: use extended GCD to get a's factor directly
    aa = integerAbs a
    ab = integerAbs b

-- | Square a Integer
integerSqr :: Integer -> Integer
integerSqr !a = integerMul a a


-- | Base 2 logarithm (floor)
--
-- For numbers <= 0, return 0
integerLog2# :: Integer -> Word#
integerLog2# (IS i)
   | isTrue# (i <=# 0#) = 0##
   | True               = wordLog2# (int2Word# i)
integerLog2# (IN _)     = 0##
integerLog2# (IP b)     = bigNatLog2# b

-- | Base 2 logarithm (floor)
--
-- For numbers <= 0, return 0
integerLog2 :: Integer -> Word
integerLog2 !i = W# (integerLog2# i)

-- | Logarithm (floor) for an arbitrary base
--
-- For numbers <= 0, return 0
integerLogBaseWord# :: Word# -> Integer -> Word#
integerLogBaseWord# base !i
   | integerIsNegative i = 0##
   | True                = naturalLogBaseWord# base (integerToNatural i)

-- | Logarithm (floor) for an arbitrary base
--
-- For numbers <= 0, return 0
integerLogBaseWord :: Word -> Integer -> Word
integerLogBaseWord (W# base) !i = W# (integerLogBaseWord# base i)

-- | Logarithm (floor) for an arbitrary base
--
-- For numbers <= 0, return 0
integerLogBase# :: Integer -> Integer -> Word#
integerLogBase# !base !i
   | integerIsNegative i = 0##
   | True                = naturalLogBase# (integerToNatural base)
                                           (integerToNatural i)

-- | Logarithm (floor) for an arbitrary base
--
-- For numbers <= 0, return 0
integerLogBase :: Integer -> Integer -> Word
integerLogBase !base !i = W# (integerLogBase# base i)

-- | Indicate if the value is a power of two and which one
integerIsPowerOf2# :: Integer -> (# () | Word# #)
integerIsPowerOf2# (IS i)
   | isTrue# (i <=# 0#) = (# () | #)
   | True               = wordIsPowerOf2# (int2Word# i)
integerIsPowerOf2# (IN _) = (# () | #)
integerIsPowerOf2# (IP w) = bigNatIsPowerOf2# w

#if WORD_SIZE_IN_BITS == 32

-- | Convert an Int64# into an Integer on 32-bit architectures
integerFromInt64# :: Int64# -> Integer
{-# NOINLINE integerFromInt64# #-}
integerFromInt64# !i
  | isTrue# ((i `leInt64#` intToInt64#  0x7FFFFFFF#)
      &&# (i `geInt64#` intToInt64# -0x80000000#))
  = IS (int64ToInt# i)

  | isTrue# (i `geInt64#` intToInt64# 0#)
  = IP (bigNatFromWord64# (int64ToWord64# i))

  | True
  = IN (bigNatFromWord64# (int64ToWord64# (negateInt64# i)))

-- | Convert a Word64# into an Integer on 32-bit architectures
integerFromWord64# :: Word64# -> Integer
{-# NOINLINE integerFromWord64# #-}
integerFromWord64# !w
  | isTrue# (w `leWord64#` wordToWord64# 0x7FFFFFFF##)
  = IS (int64ToInt# (word64ToInt64# w))
  | True
  = IP (bigNatFromWord64# w)

-- | Convert an Integer into an Int64# on 32-bit architectures
integerToInt64# :: Integer -> Int64#
{-# NOINLINE integerToInt64# #-}
integerToInt64# (IS i) = intToInt64# i
integerToInt64# (IP b) = word64ToInt64# (bigNatToWord64# b)
integerToInt64# (IN b) = negateInt64# (word64ToInt64# (bigNatToWord64# b))

-- | Convert an Integer into a Word64# on 32-bit architectures
integerToWord64# :: Integer -> Word64#
{-# NOINLINE integerToWord64# #-}
integerToWord64# (IS i) = int64ToWord64# (intToInt64# i)
integerToWord64# (IP b) = bigNatToWord64# b
integerToWord64# (IN b) = int64ToWord64# (negateInt64# (word64ToInt64# (bigNatToWord64# b)))

#else

-- | Convert an Int64# into an Integer on 64-bit architectures
integerFromInt64# :: Int# -> Integer
integerFromInt64# !x = IS x

#endif

----------------------------------------------------------------------------
-- Conversions to/from floating point
----------------------------------------------------------------------------

-- | Decode a Double# into (# Integer mantissa, Int# exponent #)
integerDecodeDouble# :: Double# -> (# Integer, Int# #)
{-# NOINLINE integerDecodeDouble# #-}
integerDecodeDouble# !x = case decodeDouble_Int64# x of
                            (# m, e #) -> (# integerFromInt64# m, e #)

-- | Decode a Double# into (# Integer mantissa, Int# exponent #)
integerDecodeDouble :: Double -> (Integer, Int)
integerDecodeDouble (D# x) = case integerDecodeDouble# x of
   (# m, e #) -> (m, I# e)

-- | Encode (# Integer mantissa, Int# exponent #) into a Double#
integerEncodeDouble# :: Integer -> Int# -> Double#
{-# NOINLINE integerEncodeDouble# #-}
integerEncodeDouble# (IS i) 0# = int2Double# i
integerEncodeDouble# (IS i) e  = intEncodeDouble# i e
integerEncodeDouble# (IP b) e  = bigNatEncodeDouble# b e
integerEncodeDouble# (IN b) e  = negateDouble# (bigNatEncodeDouble# b e)

-- | Encode (Integer mantissa, Int exponent) into a Double
integerEncodeDouble :: Integer -> Int -> Double
integerEncodeDouble !m (I# e)  = D# (integerEncodeDouble# m e)

-- | Encode an Integer (mantissa) into a Double#
integerToDouble# :: Integer -> Double#
{-# NOINLINE integerToDouble# #-}
integerToDouble# !i = integerEncodeDouble# i 0#

-- | Encode an Integer (mantissa) into a Float#
integerToFloat# :: Integer -> Float#
{-# NOINLINE integerToFloat# #-}
integerToFloat# !i = integerEncodeFloat# i 0#

-- | Encode (# Integer mantissa, Int# exponent #) into a Float#
--
-- TODO: Not sure if it's worth to write 'Float' optimized versions here
integerEncodeFloat# :: Integer -> Int# -> Float#
{-# NOINLINE integerEncodeFloat# #-}
integerEncodeFloat# !m 0# = double2Float# (integerToDouble# m)
integerEncodeFloat# !m e  = double2Float# (integerEncodeDouble# m e)

-- | Compute the number of digits of the Integer (without the sign) in the given base.
--
-- `base` must be > 1
integerSizeInBase# :: Word# -> Integer -> Word#
integerSizeInBase# base (IS i) = wordSizeInBase# base (int2Word# (absI# i))
integerSizeInBase# base (IP n) = bigNatSizeInBase# base n
integerSizeInBase# base (IN n) = bigNatSizeInBase# base n

-- | Write an 'Integer' (without sign) to @/addr/@ in base-256 representation
-- and return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToAddr# :: Integer -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
integerToAddr# (IS i) = wordToAddr# (int2Word# (absI# i))
integerToAddr# (IP n) = bigNatToAddr# n
integerToAddr# (IN n) = bigNatToAddr# n

-- | Write an 'Integer' (without sign) to @/addr/@ in base-256 representation
-- and return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToAddr :: Integer -> Addr# -> Bool# -> IO Word
integerToAddr a addr e = IO \s -> case integerToAddr# a addr e s of
   (# s', w #) -> (# s', W# w #)

-- | Read an 'Integer' (without sign) in base-256 representation from an Addr#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
integerFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, Integer #)
integerFromAddr# sz addr e s =
   case bigNatFromAddr# sz addr e s of
      (# s', n #) -> (# s', integerFromBigNat n #)

-- | Read an 'Integer' (without sign) in base-256 representation from an Addr#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
integerFromAddr :: Word# -> Addr# -> Bool# -> IO Integer
integerFromAddr sz addr e = IO (integerFromAddr# sz addr e)



-- | Write an 'Integer' (without sign) in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToMutableByteArray# :: Integer -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
integerToMutableByteArray# (IS i) = wordToMutableByteArray# (int2Word# (absI# i))
integerToMutableByteArray# (IP a) = bigNatToMutableByteArray# a
integerToMutableByteArray# (IN a) = bigNatToMutableByteArray# a

-- | Write an 'Integer' (without sign) in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToMutableByteArray :: Integer -> MutableByteArray# RealWorld -> Word# -> Bool# -> IO Word
integerToMutableByteArray i mba w e = IO \s -> case integerToMutableByteArray# i mba w e s of
   (# s', r #) -> (# s', W# r #)

-- | Read an 'Integer' (without sign) in base-256 representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
integerFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> State# s -> (# State# s, Integer #)
integerFromByteArray# sz ba off e s = case bigNatFromByteArray# sz ba off e s of
   (# s', a #) -> (# s', integerFromBigNat a #)

-- | Read an 'Integer' (without sign) in base-256 representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
integerFromByteArray :: Word# -> ByteArray# -> Word# -> Bool# -> Integer
integerFromByteArray sz ba off e = case runRW# (integerFromByteArray# sz ba off e) of
   (# _, i #) -> i
