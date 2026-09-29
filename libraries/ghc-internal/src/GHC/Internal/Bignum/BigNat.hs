{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE MonoLocalBinds #-}
   -- Local State#-passing loops (e.g. in bigNatFromAddrLE#) must stay
   -- monomorphic in the state type, otherwise GHC does not turn them into
   -- join points (see #14620).

{-# OPTIONS_GHC -fdefines-known-key-names #-}
    -- Defines lots of functions that have BuiltinRules

{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Multi-precision natural
module GHC.Internal.Bignum.BigNat where

#include "MachDeps.h"
#include "WordSize.h"

import GHC.Internal.Prim
import GHC.Internal.Types
import GHC.Internal.Classes
import GHC.Internal.Magic
import GHC.Internal.Bignum.Primitives
import GHC.Internal.Bignum.WordArray
import GHC.Internal.Bignum.Backend
import qualified GHC.Internal.Unboxed.Do as Ubx

-- | A BigNat
--
-- Represented as an array of limbs (Word#) stored in little-endian order (Word#
-- themselves use machine order).
--
-- Invariant (canonical representation): higher Word# is non-zero.
--
-- As a consequence, zero is represented with a WordArray# whose size is 0.
type BigNat# = WordArray#
   -- we use a type-alias instead of an unlifted newtype to make Integer/Natural
   -- types easier to wire in the compiler

-- | A lifted BigNat
--
-- Represented as an array of limbs (Word#) stored in little-endian order (Word#
-- themselves use machine order).
--
-- Invariant (canonical representation): higher Word# is non-zero.
--
-- As a consequence, zero is represented with a WordArray# whose size is 0.
data BigNat = BN# { unBigNat :: BigNat# }

-- Note [Why (# #)?]
-- ~~~~~~~~~~~~~~~~~
--
-- We can't have top-level BigNat# for now because they are unlifted ByteArray#
-- (see #17521). So we use functions that take an empty argument (# #) that
-- will be discarded at compile time.


-- | Check that the BigNat is valid
bigNatCheck# :: BigNat# -> Bool#
bigNatCheck# bn
   | 0#  <- bigNatSize# bn                         = 1#
   -- check that size is a multiple of Word size
   | r <- remInt# (sizeofByteArray# bn) WORD_SIZE_IN_BYTES#
   , isTrue# (r /=# 0#)                            = 0#
   -- check that most-significant limb isn't zero
   | 0## <- bigNatIndex# bn (bigNatSize# bn -# 1#) = 0#
   | True                                          = 1#

-- | Check that the BigNat is valid
bigNatCheck :: BigNat# -> Bool
bigNatCheck bn = isTrue# (bigNatCheck# bn)

-- | Number of words in the BigNat
bigNatSize :: BigNat# -> Word
bigNatSize bn = W# (int2Word# (bigNatSize# bn))

-- | Number of words in the BigNat
bigNatSize# :: BigNat# -> Int#
bigNatSize# ba = wordArraySize# ba

{-# NOINLINE bigNatZero #-}
bigNatZero :: BigNat
bigNatZero = BN# (withNewWordArray# 0# (\_ s -> s))

{-# NOINLINE bigNatOne #-}
bigNatOne :: BigNat
bigNatOne = BN# (bigNatFromWord# 1##)

-- | BigNat Zero
bigNatZero# :: (# #) -> BigNat# -- cf Note [Why (# #)?]
bigNatZero# _ = case bigNatZero of
   BN# w -> w

-- | BigNat one
bigNatOne# :: (# #) -> BigNat# -- cf Note [Why (# #)?]
bigNatOne# _ = case bigNatOne of
   BN# w -> w

raiseDivZero_BigNat :: (# #) -> BigNat#
raiseDivZero_BigNat _ = case raiseDivZero of
   !_ -> bigNatZero# (# #)
   -- see Note [ghc-bignum exceptions] in GHC.Internal.Bignum.Primitives

-- | Indicate if a bigNat is zero
bigNatIsZero :: BigNat# -> Bool
bigNatIsZero bn = isTrue# (bigNatIsZero# bn)

-- | Indicate if a bigNat is zero
bigNatIsZero# :: BigNat# -> Bool#
bigNatIsZero# ba = wordArraySize# ba ==# 0#

-- | Indicate if a bigNat is one
bigNatIsOne :: BigNat# -> Bool
bigNatIsOne bn = isTrue# (bigNatIsOne# bn)

-- | Indicate if a bigNat is one
bigNatIsOne# :: BigNat# -> Bool#
bigNatIsOne# ba =
   wordArraySize# ba ==# 1#
   &&# indexWordArray# ba 0# `eqWord#` 1##

-- | Indicate if a bigNat is two
bigNatIsTwo :: BigNat# -> Bool
bigNatIsTwo bn = isTrue# (bigNatIsTwo# bn)

-- | Indicate if a bigNat is two
bigNatIsTwo# :: BigNat# -> Bool#
bigNatIsTwo# ba =
   wordArraySize# ba ==# 1#
   &&# indexWordArray# ba 0# `eqWord#` 2##

-- | Indicate if the value is a power of two and which one
bigNatIsPowerOf2# :: BigNat# -> (# (# #) | Word# #)
bigNatIsPowerOf2# a
   | bigNatIsZero a                      = (# (# #) | #)
   | True =
    let
      msw  = bigNatIndex# a imax
      sz   = bigNatSize# a
      imax = sz -# 1#
      checkAllZeroes i
         | isTrue# (i <# 0#) = 1#
         | True = case bigNatIndex# a i of
                     0## -> checkAllZeroes (i -# 1#)
                     _   -> 0#
    in case wordIsPowerOf2# msw of
               (# (# #) | #) -> (# (# #) | #)
               (# | c  #) -> case checkAllZeroes (imax -# 1#) of
                  0# -> (# (# #) | #)
                  _  -> (# | c `plusWord#`
                              (int2Word# imax `uncheckedShiftL#` WORD_SIZE_BITS_SHIFT#) #)

-- | Return the Word# at the given index
bigNatIndex# :: BigNat# -> Int# -> Word#
bigNatIndex# x i = indexWordArray# x i

-- | Return the Word# at the given index
bigNatIndex :: BigNat# -> Int# -> Word
bigNatIndex bn i = W# (bigNatIndex# bn i)

-------------------------------------------------
-- Conversion
-------------------------------------------------

-- | Create a BigNat from a Word
bigNatFromWord :: Word -> BigNat#
bigNatFromWord (W# w) = bigNatFromWord# w

-- | Create a BigNat from a Word
bigNatFromWord# :: Word# -> BigNat#
bigNatFromWord# 0## = bigNatZero# (# #)
bigNatFromWord# w   = wordArrayFromWord# w

-- | Convert a list of non-zero Words (most-significant first) into a BigNat
bigNatFromWordList :: [Word] -> BigNat#
bigNatFromWordList (W# 0##:xs) = bigNatFromWordList xs
bigNatFromWordList xs          = bigNatFromWordListUnsafe xs

-- | Convert a list of non-zero Words (most-significant first) into a BigNat
bigNatFromWordList# :: [Word] -> WordArray#
{-# NOINLINE bigNatFromWordList# #-}
bigNatFromWordList# xs = bigNatFromWordList xs

-- | Return the absolute value of the Int# in a BigNat
bigNatFromAbsInt# :: Int# -> BigNat#
bigNatFromAbsInt# i = bigNatFromWord# (wordFromAbsInt# i)

-- | Convert a list of non-zero Words (most-significant first) into a BigNat.
-- Don't remove most-significant zero words
bigNatFromWordListUnsafe :: [Word] -> BigNat#
bigNatFromWordListUnsafe [] = bigNatZero# (# #)
bigNatFromWordListUnsafe xs =
   let
      length i []     = i
      length i (_:ys) = length (i +# 1#) ys
      !lxs = length 0# xs
      writeWordList _mwa _i []        = Ubx.nop
      writeWordList mwa   i (W# w:ws) = Ubx.do
         mwaWrite# mwa i w
         writeWordList mwa (i -# 1#) ws
   in withNewWordArray# lxs \mwa ->
            writeWordList mwa (lxs -# 1#) xs

-- | Convert a BigNat into a list of non-zero Words (most-significant first)
bigNatToWordList :: BigNat# -> [Word]
bigNatToWordList bn = go (bigNatSize# bn)
   where
      go 0# = []
      go n  = bigNatIndex bn (n -# 1#) : go (n -# 1#)


-- | Convert two Word# (most-significant first) into a BigNat
bigNatFromWord2# :: Word# -> Word# -> BigNat#
bigNatFromWord2# 0## 0## = bigNatZero# (# #)
bigNatFromWord2# 0## l   = bigNatFromWord# l
bigNatFromWord2# h   l   = wordArrayFromWord2# h l

-- | Convert a BigNat into a Word#
bigNatToWord# :: BigNat# -> Word#
bigNatToWord# a
   | bigNatIsZero a = 0##
   | True           = bigNatIndex# a 0#

-- | Convert a BigNat into a Word# if it fits
bigNatToWordMaybe# :: BigNat# -> (# (# #) | Word# #)
bigNatToWordMaybe# a
   | bigNatIsZero a                = (#       | 0## #)
   | isTrue# (bigNatSize# a ># 1#) = (# (# #) |     #)
   | True                          = (#       | bigNatIndex# a 0# #)

-- | Convert a BigNat into a Word
bigNatToWord :: BigNat# -> Word
bigNatToWord bn = W# (bigNatToWord# bn)

-- | Convert a BigNat into a Int#
bigNatToInt# :: BigNat# -> Int#
bigNatToInt# a
   | bigNatIsZero a = 0#
   | True           = indexIntArray# a 0#

-- | Convert a BigNat into a Int
bigNatToInt :: BigNat# -> Int
bigNatToInt bn = I# (bigNatToInt# bn)

#if WORD_SIZE_IN_BITS == 32

-- | Convert a Word64# into a BigNat on 32-bit architectures
bigNatFromWord64# :: Word64# -> BigNat#
bigNatFromWord64# w64 = bigNatFromWord2# wh# wl#
  where
    wh# = word64ToWord# (uncheckedShiftRL64# w64 32#)
    wl# = word64ToWord# w64

-- | Convert a BigNat into a Word64# on 32-bit architectures
bigNatToWord64# :: BigNat# -> Word64#
bigNatToWord64# b
  | bigNatIsZero b = wordToWord64# 0##
  | wl <- wordToWord64# (bigNatToWord# b)
  = if isTrue# (bigNatSize# b ># 1#)
      then
         let wh = wordToWord64# (bigNatIndex# b 1#)
         in uncheckedShiftL64# wh 32# `or64#` wl
      else wl

#else

-- | Convert a Word64# into a BigNat on 64-bit architectures
bigNatFromWord64# :: Word64# -> BigNat#
bigNatFromWord64# w64 = bigNatFromWord# (word64ToWord# w64)

-- | Convert a BigNat into a Word64# on 64-bit architectures
bigNatToWord64# :: BigNat# -> Word64#
bigNatToWord64# b = wordToWord64# (bigNatToWord# b)

#endif

-- | Encode (# BigNat mantissa, Int# exponent #) into a Double#
bigNatEncodeDouble# :: BigNat# -> Int# -> Double#
bigNatEncodeDouble# a e
   | bigNatIsZero a
   = word2Double# 0## -- FIXME: isn't it NaN on 0# exponent?

   | True
   = inline bignat_encode_double a e

-------------------------------------------------
-- Predicates
-------------------------------------------------

-- | Test if a BigNat is greater than a Word
bigNatGtWord# :: BigNat# -> Word# -> Bool#
bigNatGtWord# bn w =
   notB# (bigNatIsZero# bn)
   &&# (   bigNatSize# bn ># 1#
       ||# bigNatIndex# bn 0# `gtWord#` w
       )

-- | Test if a BigNat is equal to a Word
bigNatEqWord# :: BigNat# -> Word# -> Bool#
bigNatEqWord# bn w
   | 0## <- w
   = bigNatIsZero# bn

   | isTrue# (bigNatSize# bn ==# 1#)
   = bigNatIndex# bn 0# `eqWord#` w

   | True
   = 0#

-- | Test if a BigNat is greater than a Word
bigNatGtWord :: BigNat# -> Word -> Bool
bigNatGtWord bn (W# w) = isTrue# (bigNatGtWord# bn w)

-- | Test if a BigNat is lower than or equal to a Word
bigNatLeWord# :: BigNat# -> Word# -> Bool#
bigNatLeWord# bn w = notB# (bigNatGtWord# bn w)

-- | Test if a BigNat is lower than or equal to a Word
bigNatLeWord :: BigNat# -> Word -> Bool
bigNatLeWord bn (W# w) = isTrue# (bigNatLeWord# bn w)

-- | Equality test for BigNat
bigNatEq# :: BigNat# -> BigNat# -> Bool#
{-# NOINLINE bigNatEq# #-}
bigNatEq# wa wb
   | isTrue# (wordArraySize# wa /=# wordArraySize# wb) = 0#
   | isTrue# (wordArraySize# wa ==# 0#)                = 1#
   | True = inline bignat_compare wa wb ==# 0#

-- | Equality test for BigNat
bigNatEq :: BigNat# -> BigNat# -> Bool
bigNatEq a b = isTrue# (bigNatEq# a b)

-- | Inequality test for BigNat
bigNatNe# :: BigNat# -> BigNat# -> Bool#
bigNatNe# a b = notB# (bigNatEq# a b)

-- | Equality test for BigNat
bigNatNe :: BigNat# -> BigNat# -> Bool
bigNatNe a b = isTrue# (bigNatNe# a b)

-- | Compare a BigNat and a Word#
bigNatCompareWord# :: BigNat# -> Word# -> Ordering
{-# NOINLINE bigNatCompareWord# #-}
bigNatCompareWord# a b
   | bigNatIsZero a                   = cmpW# 0## b
   | isTrue# (wordArraySize# a ># 1#) = GT
   | True
   = cmpW# (indexWordArray# a 0#) b

-- | Compare a BigNat and a Word
bigNatCompareWord :: BigNat# -> Word -> Ordering
bigNatCompareWord a (W# b) = bigNatCompareWord# a b

-- | Compare two BigNat
bigNatCompare :: BigNat# -> BigNat# -> Ordering
{-# NOINLINE bigNatCompare #-}
bigNatCompare a b =
   let
      szA = wordArraySize# a
      szB = wordArraySize# b
   in if
   | isTrue# (szA ># szB) -> GT
   | isTrue# (szA <# szB) -> LT
   | isTrue# (szA ==# 0#) -> EQ
   | True                 -> compareInt# (inline bignat_compare a b) 0#


-- | Predicate: a < b
bigNatLt# :: BigNat# -> BigNat# -> Bool#
bigNatLt# a b
  | LT <- bigNatCompare a b = 1#
  | True                    = 0#

-- | Predicate: a < b
bigNatLt :: BigNat# -> BigNat# -> Bool
bigNatLt a b = isTrue# (bigNatLt# a b)

-- | Predicate: a <= b
bigNatLe# :: BigNat# -> BigNat# -> Bool#
bigNatLe# a b
  | GT <- bigNatCompare a b = 0#
  | True                    = 1#

-- | Predicate: a <= b
bigNatLe :: BigNat# -> BigNat# -> Bool
bigNatLe a b = isTrue# (bigNatLe# a b)

-- | Predicate: a > b
bigNatGt# :: BigNat# -> BigNat# -> Bool#
bigNatGt# a b
  | GT <- bigNatCompare a b = 1#
  | True                    = 0#

-- | Predicate: a > b
bigNatGt :: BigNat# -> BigNat# -> Bool
bigNatGt a b = isTrue# (bigNatGt# a b)

-- | Predicate: a >= b
bigNatGe# :: BigNat# -> BigNat# -> Bool#
bigNatGe# a b
  | LT <- bigNatCompare a b = 0#
  | True                    = 1#

-- | Predicate: a >= b
bigNatGe :: BigNat# -> BigNat# -> Bool
bigNatGe a b = isTrue# (bigNatGe# a b)

-------------------------------------------------
-- Addition
-------------------------------------------------

-- | Add a bigNat and a Word#
bigNatAddWord# :: BigNat# -> Word# -> BigNat#
bigNatAddWord# a b
   | 0## <- b
   = a

   | bigNatIsZero a
   = bigNatFromWord# b

   | True
   = withNewWordArrayTrimmed# (wordArraySize# a +# 1#) \mwa s ->
         inline bignat_add_word mwa a b s

-- | Add a bigNat and a Word
bigNatAddWord :: BigNat# -> Word -> BigNat#
bigNatAddWord a (W# b) = bigNatAddWord# a b

-- | Add two bigNats
bigNatAdd :: BigNat# -> BigNat# -> BigNat#
bigNatAdd a b
   | bigNatIsZero a = b
   | bigNatIsZero b = a
   | True =
   let
      !szA     = wordArraySize# a
      !szB     = wordArraySize# b
      !szMax   = maxI# szA szB
      !sz      = szMax +# 1# -- for the potential carry
   in withNewWordArrayTrimmed# sz \mwa s ->
         inline bignat_add mwa a b s

-------------------------------------------------
-- Multiplication
-------------------------------------------------

-- | Multiply a BigNat by a Word#
bigNatMulWord# :: BigNat# -> Word# -> BigNat#
bigNatMulWord# a w
   | 0## <- w       = bigNatZero# (# #)
   | 1## <- w       = a
   | bigNatIsZero a = bigNatZero# (# #)
   | bigNatIsOne  a = bigNatFromWord# w
   | isTrue# (bigNatSize# a ==# 1#)
   = case timesWord2# (bigNatIndex# a 0#) w of
      (# h, l #) -> bigNatFromWord2# h l
   | True = withNewWordArrayTrimmed# (bigNatSize# a +# 1#) \mwa s ->
               inline bignat_mul_word mwa a w s

-- | Multiply a BigNAt by a Word
bigNatMulWord :: BigNat# -> Word -> BigNat#
bigNatMulWord a (W# w) = bigNatMulWord# a w

-- | Square a BigNat
bigNatSqr :: BigNat# -> BigNat#
bigNatSqr a = bigNatMul a a
   -- This can be replaced by a backend primitive in the future (e.g. to use
   -- GMP's mpn_sqr)

-- | Multiplication (classical algorithm)
bigNatMul :: BigNat# -> BigNat# -> BigNat#
bigNatMul a b
   | bigNatSize b > bigNatSize a = bigNatMul b a -- optimize loops
   | bigNatIsZero a = a
   | bigNatIsZero b = b
   | bigNatIsOne  a = b
   | bigNatIsOne  b = a
   | True =
      let
         !szA = wordArraySize# a
         !szB = wordArraySize# b
         !sz  = szA +# szB
      in withNewWordArrayTrimmed# sz \mwa s->
            inline bignat_mul mwa a b s


-------------------------------------------------
-- Subtraction
-------------------------------------------------

-- | Subtract a Word# from a BigNat
--
-- The BigNat must be bigger than the Word#.
bigNatSubWordUnsafe# :: BigNat# -> Word# -> BigNat#
bigNatSubWordUnsafe# x y
   | 0## <- y = x
   | True     = withNewWordArrayTrimmed# sz \mwa -> go mwa y 0#
   where
      !sz = wordArraySize# x

      go mwa carry i
         | isTrue# (i >=# sz)
         = Ubx.nop

         | 0## <- carry
         = mwaArrayCopy# mwa i x i (sz -# i)

         | True
         = Ubx.do
            let !(# l, c #) = subWordC# (indexWordArray# x i) carry
            mwaWrite# mwa i l
            go mwa (int2Word# c) (i +# 1#)

-- | Subtract a Word# from a BigNat
--
-- The BigNat must be bigger than the Word#.
bigNatSubWordUnsafe :: BigNat# -> Word -> BigNat#
bigNatSubWordUnsafe x (W# y) = bigNatSubWordUnsafe# x y

-- | Subtract a Word# from a BigNat
bigNatSubWord# :: BigNat# -> Word# -> (# (# #) | BigNat# #)
bigNatSubWord# a b
   | 0## <- b          = (# | a #)
   | bigNatIsZero a    = (# (# #) | #)
   | True
   = withNewWordArrayTrimmedMaybe# (bigNatSize# a) \mwa s ->
            inline bignat_sub_word mwa a b s


-- | Subtract two BigNat (don't check if a >= b)
bigNatSubUnsafe :: BigNat# -> BigNat# -> BigNat#
bigNatSubUnsafe a b
   | bigNatIsZero b = a
   | True =
      let szA = wordArraySize# a
      in withNewWordArrayTrimmed# szA \mwa -> Ubx.do
            ok <- inline bignat_sub mwa a b
            case ok of
               1# -> Ubx.nop
               _  -> case raiseUnderflow of
                        !_ -> Ubx.nop
                        -- see Note [ghc-bignum exceptions] in
                        -- GHC.Internal.Bignum.Primitives

-- | Subtract two BigNat
bigNatSub :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)
bigNatSub a b
   | bigNatIsZero b = (# | a #)
   | isTrue# (bigNatSize# a <# bigNatSize# b)
   = (# (# #) | #)

   | True
   = withNewWordArrayTrimmedMaybe# (bigNatSize# a) \mwa s ->
            inline bignat_sub mwa a b s


-------------------------------------------------
-- Division
-------------------------------------------------

-- | Divide a BigNat by a Word, return the quotient
--
-- Require:
--    b /= 0
bigNatQuotWord# :: BigNat# -> Word# -> BigNat#
bigNatQuotWord# a b
   | 1## <- b = a
   | 0## <- b = raiseDivZero_BigNat (# #)
   | True =
   let
      sz = wordArraySize# a
   in withNewWordArrayTrimmed# sz \mwq s ->
         inline bignat_quot_word mwq a b s

-- | Divide a BigNat by a Word, return the quotient
--
-- Require:
--    b /= 0
bigNatQuotWord :: BigNat# -> Word -> BigNat#
bigNatQuotWord a (W# b) = bigNatQuotWord# a b

-- | Divide a BigNat by a Word, return the remainder
--
-- Require:
--    b /= 0
bigNatRemWord# :: BigNat# -> Word# -> Word#
bigNatRemWord# a b
   | 0## <- b       = raiseDivZero_Word# (# #)
   | 1## <- b       = 0##
   | bigNatIsZero a = 0##
   | True           = inline bignat_rem_word a b

-- | Divide a BigNat by a Word, return the remainder
--
-- Require:
--    b /= 0
bigNatRemWord :: BigNat# -> Word -> Word
bigNatRemWord a (W# b) = W# (bigNatRemWord# a b)

-- | QuotRem a BigNat by a Word
--
-- Require:
--    b /= 0
bigNatQuotRemWord# :: BigNat# -> Word# -> (# BigNat#, Word# #)
bigNatQuotRemWord# a b
   | 0## <- b = case raiseDivZero of
                  !_ -> (# bigNatZero# (# #), 0## #)
                  -- see Note [ghc-bignum exceptions] in GHC.Internal.Bignum.Primitives
   | 1## <- b = (# a, 0## #)
   | isTrue# (bigNatSize# a ==# 1#)
   , a0 <- indexWordArray# a 0#
   = case compareWord# a0 b of
      LT -> (# bigNatZero# (# #), a0  #)
      EQ -> (# bigNatOne#  (# #), 0## #)
      GT -> case quotRemWord# a0 b of
               (# q, r #) -> (# bigNatFromWord# q, r #)
   | True =
   let
      sz = wordArraySize# a
      io = Ubx.do
         mwq <- newWordArray# sz
         r <- inline bignat_quotrem_word mwq a b
         mwaTrimZeroes# mwq
         wq <- unsafeFreezeByteArray# mwq
         Ubx.pure (# wq, r #)
   in case runRW# io of
         (# _, (# wq,r #) #) -> (# wq, r #)


-- | BigNat division returning (quotient,remainder)
bigNatQuotRem# :: BigNat# -> BigNat# -> (# BigNat#, BigNat# #)
bigNatQuotRem# a b
   | bigNatIsZero b          = case raiseDivZero of
                                 !_ -> (# bigNatZero# (# #), bigNatZero# (# #) #)
                                 -- see Note [ghc-bignum exceptions] in GHC.Internal.Bignum.Primitives
   | bigNatIsZero a          = (# bigNatZero# (# #), bigNatZero# (# #) #)
   | bigNatIsOne b           = (# a                , bigNatZero# (# #) #)
   | LT <- cmp               = (# bigNatZero# (# #), a #)
   | EQ <- cmp               = (# bigNatOne#  (# #), bigNatZero# (# #) #)
   | isTrue# (szB ==# 1#)    = case bigNatQuotRemWord# a (bigNatIndex# b 0#) of
                                 (# q, r #) -> (# q, bigNatFromWord# r #)

   | True = withNewWordArray2Trimmed# szQ szR \mwq mwr s ->
                     inline bignat_quotrem mwq mwr a b s
   where
   cmp = bigNatCompare a b
   szA = wordArraySize# a
   szB = wordArraySize# b
   szQ = 1# +# szA -# szB
   szR = szB


-- | BigNat division returning quotient
bigNatQuot :: BigNat# -> BigNat# -> BigNat#
bigNatQuot a b
   | bigNatIsZero b          = raiseDivZero_BigNat (# #)
   | bigNatIsZero a          = bigNatZero# (# #)
   | bigNatIsOne b           = a
   | LT <- cmp               = bigNatZero# (# #)
   | EQ <- cmp               = bigNatOne# (# #)
   | isTrue# (szB ==# 1#)    = bigNatQuotWord# a (bigNatIndex# b 0#)
   | True                    = withNewWordArrayTrimmed# szQ \mwq s ->
                                 inline bignat_quot mwq a b s
   where
   cmp = bigNatCompare a b
   szA = wordArraySize# a
   szB = wordArraySize# b
   szQ = 1# +# szA -# szB

-- | BigNat division returning remainder
bigNatRem :: BigNat# -> BigNat# -> BigNat#
bigNatRem a b
   | bigNatIsZero b          = raiseDivZero_BigNat (# #)
   | bigNatIsZero a          = bigNatZero# (# #)
   | bigNatIsOne b           = bigNatZero# (# #)
   | LT <- cmp               = a
   | EQ <- cmp               = bigNatZero# (# #)
   | isTrue# (szB ==# 1#)    = case bigNatRemWord# a (bigNatIndex# b 0#) of
                                 r -> bigNatFromWord# r
   | True                    = withNewWordArrayTrimmed# szR \mwr s ->
                                 inline bignat_rem mwr a b s
   where
   cmp = bigNatCompare a b
   szB = wordArraySize# b
   szR = szB

-------------------------------------------------
-- GCD / LCM
-------------------------------------------------

-- Word#/Int# GCDs shouldn't be here in BigNat. However GMP provides a very fast
-- implementation so we keep this here at least until we get a native Haskell
-- implementation as fast as GMP's one. Note that these functions are used in
-- `base` (e.g. in GHC.Real)

-- | Greatest common divisor between two Word#
gcdWord# :: Word# -> Word# -> Word#
gcdWord# = bignat_gcd_word_word

-- | Greatest common divisor between two Word
gcdWord :: Word -> Word -> Word
gcdWord (W# x) (W# y) = W# (gcdWord# x y)

-- | Greatest common divisor between two Int#
--
-- __Warning__: result may become negative if (at least) one argument
-- is 'minBound'
gcdInt# :: Int# -> Int# -> Int#
gcdInt# x y = word2Int# (gcdWord# (wordFromAbsInt# x) (wordFromAbsInt# y))

-- | Greatest common divisor between two Int
--
-- __Warning__: result may become negative if (at least) one argument
-- is 'minBound'
gcdInt :: Int -> Int -> Int
gcdInt (I# x) (I# y) = I# (gcdInt# x y)

-- | Greatest common divisor
bigNatGcd :: BigNat# -> BigNat# -> BigNat#
bigNatGcd a b
   | bigNatIsZero a = b
   | bigNatIsZero b = a
   | bigNatIsOne a  = a
   | bigNatIsOne b  = b
   | True
   = case (# bigNatSize# a, bigNatSize# b #) of
      (# 1#, 1# #) -> bigNatFromWord# (gcdWord# (bigNatIndex# a 0#)
                                                (bigNatIndex# b 0#))
      (# 1#, _  #) -> bigNatFromWord# (bigNatGcdWord# b (bigNatIndex# a 0#))
      (# _ , 1# #) -> bigNatFromWord# (bigNatGcdWord# a (bigNatIndex# b 0#))
      _            ->
         let
            go wx wy = -- wx > wy
               withNewWordArrayTrimmed# (wordArraySize# wy) \mwr s ->
                  bignat_gcd mwr wx wy s
         in case bigNatCompare a b of
               EQ -> a
               LT -> go b a
               GT -> go a b

-- | Greatest common divisor
bigNatGcdWord# :: BigNat# -> Word# -> Word#
bigNatGcdWord# a b
   | bigNatIsZero a = 0##
   | 0## <- b       = 0##
   | bigNatIsOne a  = 1##
   | 1## <- b       = 1##
   | True           = case bigNatCompareWord# a b of
      EQ -> b
      _  -> bignat_gcd_word a b

-- | Least common multiple
bigNatLcm :: BigNat# -> BigNat# -> BigNat#
bigNatLcm a b
   | bigNatIsZero a = bigNatZero# (# #)
   | bigNatIsZero b = bigNatZero# (# #)
   | bigNatIsOne  a = b
   | bigNatIsOne  b = a
   | True
   = case (# bigNatSize# a, bigNatSize# b #) of
      (# 1#, 1# #) -> bigNatLcmWordWord# (bigNatIndex# a 0#) (bigNatIndex# b 0#)
      (# 1#, _  #) -> bigNatLcmWord# b (bigNatIndex# a 0#)
      (# _ , 1# #) -> bigNatLcmWord# a (bigNatIndex# b 0#)
      _            -> (a `bigNatQuot` (a `bigNatGcd` b)) `bigNatMul` b
                       -- TODO: use extended GCD to get a's factor directly

-- | Least common multiple with a Word#
bigNatLcmWord# :: BigNat# -> Word# -> BigNat#
bigNatLcmWord# a b
   | bigNatIsZero a      = bigNatZero# (# #)
   | 0## <- b            = bigNatZero# (# #)
   | bigNatIsOne  a      = bigNatFromWord# b
   | 1## <- b            = a
   | 1# <- bigNatSize# a = bigNatLcmWordWord# (bigNatIndex# a 0#) b
   | True
   = (a `bigNatQuotWord#` (a `bigNatGcdWord#` b)) `bigNatMulWord#` b
      -- TODO: use extended GCD to get a's factor directly

-- | Least common multiple between two Word#
bigNatLcmWordWord# :: Word# -> Word# -> BigNat#
bigNatLcmWordWord# a b
   | 0## <- a = bigNatZero# (# #)
   | 0## <- b = bigNatZero# (# #)
   | 1## <- a = bigNatFromWord# b
   | 1## <- b = bigNatFromWord# a
   | True     = case (a `quotWord#` (a `gcdWord#` b)) `timesWord2#` b of
                     -- TODO: use extended GCD to get a's factor directly
      (# h, l #) -> bigNatFromWord2# h l


-------------------------------------------------
-- Bitwise operations
-------------------------------------------------

-- | Bitwise OR
bigNatOr :: BigNat# -> BigNat# -> BigNat#
bigNatOr a b
   | bigNatIsZero a = b
   | bigNatIsZero b = a
   | True           = withNewWordArray# sz \mwa s ->
                        inline bignat_or mwa a b s
   where
      !szA = wordArraySize# a
      !szB = wordArraySize# b
      !sz  = maxI# szA szB

-- | Bitwise OR with Word#
bigNatOrWord# :: BigNat# -> Word# -> BigNat#
bigNatOrWord# a b
   | bigNatIsZero a = bigNatFromWord# b
   | 0## <- b       = a
   | True           =
      let sz = wordArraySize# a
      in withNewWordArray# sz \mwa -> Ubx.do
            mwaArrayCopy# mwa 1# a 1# (sz -# 1#)
            mwaWrite# mwa 0# (indexWordArray# a 0# `or#` b)

-- | Bitwise AND
bigNatAnd :: BigNat# -> BigNat# -> BigNat#
bigNatAnd a b
   | bigNatIsZero a = a
   | bigNatIsZero b = b
   | True           = withNewWordArrayTrimmed# sz \mwa s ->
                        inline bignat_and mwa a b s
   where
      !szA = wordArraySize# a
      !szB = wordArraySize# b
      !sz  = minI# szA szB

-- | Bitwise ANDNOT
bigNatAndNot :: BigNat# -> BigNat# -> BigNat#
bigNatAndNot a b
   | bigNatIsZero a = a
   | bigNatIsZero b = a
   | True           = withNewWordArrayTrimmed# szA \mwa s ->
                        inline bignat_and_not mwa a b s
   where
      !szA = wordArraySize# a

-- | Bitwise AND with Word#
bigNatAndWord# :: BigNat# -> Word# -> BigNat#
bigNatAndWord# a b
   | bigNatIsZero a = a
   | True           = bigNatFromWord# (indexWordArray# a 0# `and#` b)

-- | Bitwise ANDNOT with Word#
bigNatAndNotWord# :: BigNat# -> Word# -> BigNat#
bigNatAndNotWord# a b
   | bigNatIsZero a     = a
   | szA <- bigNatSize# a
   = withNewWordArray# szA \mwa -> Ubx.do
      -- duplicate higher limbs
      mwaArrayCopy# mwa 1# a 1# (szA -# 1#)
      writeWordArray# mwa 0# (indexWordArray# a 0# `and#` not# b)

-- | Bitwise AND with Int#
bigNatAndInt# :: BigNat# -> Int# -> BigNat#
bigNatAndInt# a b
   | bigNatIsZero a     = a
   | isTrue# (b >=# 0#) = bigNatAndWord# a (int2Word# b)
   | szA <- bigNatSize# a
   = withNewWordArray# szA \mwa -> Ubx.do
      -- duplicate higher limbs (because of sign-extension of b)
      mwaArrayCopy# mwa 1# a 1# (szA -# 1#)
      writeWordArray# mwa 0# (indexWordArray# a 0# `and#` int2Word# b)


-- | Bitwise XOR
bigNatXor :: BigNat# -> BigNat# -> BigNat#
bigNatXor a b
   | bigNatIsZero a = b
   | bigNatIsZero b = a
   | True           = withNewWordArrayTrimmed# sz \mwa s ->
                        inline bignat_xor mwa a b s
   where
      !szA = wordArraySize# a
      !szB = wordArraySize# b
      !sz  = maxI# szA szB

-- | Bitwise XOR with Word#
bigNatXorWord# :: BigNat# -> Word# -> BigNat#
bigNatXorWord# a b
   | bigNatIsZero a = bigNatFromWord# b
   | 0## <- b       = a
   | True           =
      let
         sz = wordArraySize# a
      in withNewWordArray# sz \mwa -> Ubx.do
            mwaArrayCopy# mwa 1# a 1# (sz -# 1#)
            mwaWrite# mwa 0# (indexWordArray# a 0# `xor#` b)

-- | PopCount for BigNat
bigNatPopCount :: BigNat# -> Word
bigNatPopCount a = W# (bigNatPopCount# a)

-- | PopCount for BigNat
bigNatPopCount# :: BigNat# -> Word#
bigNatPopCount# a
   | bigNatIsZero a = 0##
   | True           = inline bignat_popcount a

-- | Bit shift right
bigNatShiftR# :: BigNat# -> Word# -> BigNat#
bigNatShiftR# a n
   | 0## <- n
   = a

   | isTrue# (wordArraySize# a ==# 0#)
   = a

   | nw <- word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
   , isTrue# (nw >=# wordArraySize# a)
   = bigNatZero# (# #)

   | True
   = let
      !szA = wordArraySize# a
      !nw  = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !sz  = szA -# nw
     in withNewWordArrayTrimmed# sz \mwa s ->
         inline bignat_shiftr mwa a n s

-- | Bit shift right (two's complement)
bigNatShiftRNeg# :: BigNat# -> Word# -> BigNat#
bigNatShiftRNeg# a n
   | 0## <- n
   = a

   | isTrue# (wordArraySize# a ==# 0#)
   = a

   | nw <- word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
   , isTrue# (nw >=# wordArraySize# a)
   = bigNatZero# (# #)

   | True
   = let
      !szA = wordArraySize# a
      !nw  = (word2Int# n -# 1#) `uncheckedIShiftRL#` WORD_SIZE_BITS_SHIFT#
      !sz  = szA -# nw
     in withNewWordArrayTrimmed# sz \mwa s ->
         inline bignat_shiftr_neg mwa a n s


-- | Bit shift right
bigNatShiftR :: BigNat# -> Word -> BigNat#
bigNatShiftR a (W# n) = bigNatShiftR# a n

-- | Bit shift left
bigNatShiftL :: BigNat# -> Word -> BigNat#
bigNatShiftL a (W# n) = bigNatShiftL# a n

-- | Bit shift left
bigNatShiftL# :: BigNat# -> Word# -> BigNat#
bigNatShiftL# a n
   | 0## <- n
   = a

   | isTrue# (wordArraySize# a ==# 0#)
   = a

   | True
   = let
      !szA = wordArraySize# a
      !nw  = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb  = word2Int# (n `and#` WORD_SIZE_BITS_MASK##)
      !sz   = szA +# nw +# (nb /=# 0#)

     in withNewWordArrayTrimmed# sz \mwa s ->
         inline bignat_shiftl mwa a n s


-- | BigNat bit test
bigNatTestBit# :: BigNat# -> Word# -> Bool#
bigNatTestBit# a n =
   let
      !sz = wordArraySize# a
      !nw = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb = n `and#` WORD_SIZE_BITS_MASK##
   in if
      | isTrue# (nw >=# sz) -> 0#
      | True                -> testBitW# (indexWordArray# a nw) nb

-- | BigNat bit test
bigNatTestBit :: BigNat# -> Word -> Bool
bigNatTestBit a (W# n) = isTrue# (bigNatTestBit# a n)


-- | Return a BigNat whose bit `i` is the only one set.
--
-- Specialized version of `bigNatShiftL (bigNatFromWord# 1##)`
--
bigNatBit# :: Word# -> BigNat#
bigNatBit# i
   | 0## <- i = bigNatOne# (# #)
   | True =
   let
      !nw = word2Int# (i `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb = word2Int# (i `and#` WORD_SIZE_BITS_MASK##)
      !sz = nw +# 1#
      !v  = 1## `uncheckedShiftL#` nb
   in withNewWordArray# sz \mwa -> Ubx.do
         -- clear the array
         mwaFill# mwa 0## 0## (int2Word# sz)
         -- set the bit in the most-significant word
         mwaWrite# mwa (sz -# 1#) v

-- | Return a BigNat whose bit `i` is the only one set.
--
-- Specialized version of `bigNatShiftL (bigNatFromWord# 1##)`
--
bigNatBit :: Word -> BigNat#
bigNatBit (W# i) = bigNatBit# i

-- | BigNat clear bit
bigNatClearBit# :: BigNat# -> Word# -> BigNat#
bigNatClearBit# a n
   -- check the range validity and the current bit value
   | isTrue# (bigNatTestBit# a n ==# 0#) = a
   | True
   = let
      !sz = wordArraySize# a
      !nw = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb = word2Int# (n `and#` WORD_SIZE_BITS_MASK##)
      !nv = bigNatIndex# a nw `xor#` bitW# nb
   in if
      | isTrue# (sz ==# 1#)
      -> bigNatFromWord# nv

      -- special case, operating on most-significant Word
      | 0## <- nv
      , isTrue# (nw +# 1# ==# sz)
      -> case sz -# (waClzAt a (sz -# 2#) +# 1#) of
            0#  -> bigNatZero# (# #)
            nsz -> withNewWordArray# nsz \mwa s ->
                     mwaArrayCopy# mwa 0# a 0# nsz s

      | True ->
         withNewWordArray# sz \mwa -> Ubx.do
            mwaArrayCopy# mwa 0# a 0# sz
            writeWordArray# mwa nw nv

-- | BigNat set bit
bigNatSetBit# :: BigNat# -> Word# -> BigNat#
{-# NOINLINE bigNatSetBit# #-}
bigNatSetBit# a n
   -- check the current bit value
   | isTrue# (bigNatTestBit# a n) = a
   | True
   = let
      !sz = wordArraySize# a
      !nw = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb = word2Int# (n `and#` WORD_SIZE_BITS_MASK##)
      d   = nw +# 1# -# sz
   in if
      -- result BigNat will have more limbs
      | isTrue# (d ># 0#)
      -> withNewWordArray# (nw +# 1#) \mwa -> Ubx.do
            mwaArrayCopy# mwa 0# a 0# sz
            mwaFill# mwa 0## (int2Word# sz) (int2Word# (d -# 1#))
            writeWordArray# mwa nw (bitW# nb)

      | nv <- bigNatIndex# a nw `or#` bitW# nb
      -> withNewWordArray# sz \mwa -> Ubx.do
            mwaArrayCopy# mwa 0# a 0# sz
            writeWordArray# mwa nw nv

-- | Reverse the given bit
bigNatComplementBit# :: BigNat# -> Word# -> BigNat#
bigNatComplementBit# a n =
   let
      !sz = wordArraySize# a
      !nw = word2Int# (n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#)
      !nb = word2Int# (n `and#` WORD_SIZE_BITS_MASK##)
      d   = nw +# 1# -# sz
   in if
      -- result BigNat will have more limbs
      | isTrue# (d ># 0#)
      -> withNewWordArray# (nw +# 1#) \mwa -> Ubx.do
            mwaArrayCopy# mwa 0# a 0# sz
            mwaFill# mwa 0## (int2Word# sz) (int2Word# (d -# 1#))
            writeWordArray# mwa nw (bitW# nb)

      | nv <- bigNatIndex# a nw `xor#` bitW# nb
      -> withNewWordArrayTrimmed# sz \mwa -> Ubx.do
            mwaArrayCopy# mwa 0# a 0# sz
            writeWordArray# mwa nw nv

-------------------------------------------------
-- Log operations
-------------------------------------------------

-- | Base 2 logarithm
bigNatLog2# :: BigNat# -> Word#
bigNatLog2# a
   | bigNatIsZero a = 0##
   | True           =
      let i = int2Word# (bigNatSize# a) `minusWord#` 1##
      in wordLog2# (bigNatIndex# a (word2Int# i))
         `plusWord#` (i `uncheckedShiftL#` WORD_SIZE_BITS_SHIFT#)

-- | Base 2 logarithm
bigNatLog2 :: BigNat# -> Word
bigNatLog2 a = W# (bigNatLog2# a)

-- | Logarithm for an arbitrary base
bigNatLogBase# :: BigNat# -> BigNat# -> Word#
bigNatLogBase# base a
   | bigNatIsZero base || bigNatIsOne base
   = unexpectedValue_Word# (# #)

   | 1# <- bigNatSize# base
   , 2## <- bigNatIndex# base 0#
   = bigNatLog2# a

   -- TODO: optimize log base power of 2 (256, etc.)

   | True
   = case go base of (# _, e' #) -> e'
   where
      go pw = if a `bigNatLt` pw
         then (# a, 0## #)
         else case go (bigNatSqr pw) of
          (# q, e #) -> if q `bigNatLt` pw
            then (# q, 2## `timesWord#` e #)
            else (# q `bigNatQuot` pw
                 , (2## `timesWord#` e) `plusWord#` 1## #)

-- | Logarithm for an arbitrary base
bigNatLogBase :: BigNat# -> BigNat# -> Word
bigNatLogBase base a = W# (bigNatLogBase# base a)

-- | Logarithm for an arbitrary base
bigNatLogBaseWord# :: Word# -> BigNat# -> Word#
bigNatLogBaseWord# base a
   | 0## <- base = unexpectedValue_Word# (# #)
   | 1## <- base = unexpectedValue_Word# (# #)
   | 2## <- base = bigNatLog2# a
   -- TODO: optimize log base power of 2 (256, etc.)
   | True = bigNatLogBase# (bigNatFromWord# base) a

-- | Logarithm for an arbitrary base
bigNatLogBaseWord :: Word -> BigNat# -> Word
bigNatLogBaseWord (W# base) a = W# (bigNatLogBaseWord# base a)

-------------------------------------------------
-- Various
-------------------------------------------------

-- | Compute the number of digits of the BigNat in the given base.
--
-- `base` must be > 1
bigNatSizeInBase# :: Word# -> BigNat# -> Word#
bigNatSizeInBase# base a
   | isTrue# (base `leWord#` 1##)
   = unexpectedValue_Word# (# #)

   | bigNatIsZero a
   = 0##

   | True
   = bigNatLogBaseWord# base a `plusWord#` 1##

-- | Compute the number of digits of the BigNat in the given base.
--
-- `base` must be > 1
bigNatSizeInBase :: Word -> BigNat# -> Word
bigNatSizeInBase (W# w) a = W# (bigNatSizeInBase# w a)

-------------------------------------------------
-- PowMod
-------------------------------------------------

-- Word# powMod shouldn't be here in BigNat. However GMP provides a very fast
-- implementation so we keep this here at least until we get a native Haskell
-- implementation as fast as GMP's one.

powModWord# :: Word# -> Word# -> Word# -> Word#
powModWord# = bignat_powmod_words


-- | \"@'bigNatPowModWord#' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
bigNatPowModWord# :: BigNat# -> BigNat# -> Word# -> Word#
bigNatPowModWord# !_ !_ 0## = raiseDivZero_Word# (# #)
bigNatPowModWord# _  _  1## = 0##
bigNatPowModWord# b  e  m
   | bigNatIsZero e         = 1##
   | bigNatIsZero b         = 0##
   | bigNatIsOne  b         = 1##
   | True                   = bignat_powmod_word b e m

-- | \"@'bigNatPowMod' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
bigNatPowMod :: BigNat# -> BigNat# -> BigNat# -> BigNat#
bigNatPowMod !b !e !m
   | (# | m' #) <- bigNatToWordMaybe# m
   = bigNatFromWord# (bigNatPowModWord# b e m')
   | bigNatIsZero m = raiseDivZero_BigNat (# #)
   | bigNatIsOne  m = bigNatFromWord# 0##
   | bigNatIsZero e = bigNatFromWord# 1##
   | bigNatIsZero b = bigNatFromWord# 0##
   | bigNatIsOne  b = bigNatFromWord# 1##
   | True           = withNewWordArrayTrimmed# (bigNatSize# m) \mwa s ->
                         inline bignat_powmod mwa b e m s

-- | Return count of trailing zero bits
--
-- Return 0 for zero BigNat
bigNatCtz# :: BigNat# -> Word#
bigNatCtz# a
   | bigNatIsZero a = 0##
   | True           = go 0# 0##
      where
         go i c = case indexWordArray# a i of
            0## -> go (i +# 1#) (c `plusWord#` WORD_SIZE_IN_BITS##)
            w   -> ctz# w `plusWord#` c

-- | Return count of trailing zero bits
--
-- Return 0 for zero BigNat
bigNatCtz :: BigNat# -> Word
bigNatCtz a = W# (bigNatCtz# a)


-- | Return count of trailing zero words
--
-- Return 0 for zero BigNat
bigNatCtzWord# :: BigNat# -> Word#
bigNatCtzWord# a
   | bigNatIsZero a = 0##
   | True           = go 0# 0##
      where
         go i c = case indexWordArray# a i of
            0## -> go (i +# 1#) (c `plusWord#` 1##)
            _   -> c

-- | Return count of trailing zero words
--
-- Return 0 for zero BigNat
bigNatCtzWord :: BigNat# -> Word
bigNatCtzWord a = W# (bigNatCtzWord# a)

-------------------------------------------------
-- Export to memory
-------------------------------------------------

-- | Write a BigNat in base-256 little-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddrLE# :: BigNat# -> Addr# -> State# s -> (# State# s, Word# #)
{-# NOINLINE bigNatToAddrLE# #-}
bigNatToAddrLE# a addr
   | isTrue# (sz ==# 0#) = Ubx.pure 0##
   | True = Ubx.do
      k <- writeMSB
      go 0#
      Ubx.pure (k `plusWord#` (int2Word# li `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#))
   where
     !sz = wordArraySize# a
     !li = sz -# 1#

     writeMSB = wordToAddrLE# (indexWordArray# a li)
                  (addr `plusAddr#` (li `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#))

     go i
      | isTrue# (i <# li)
      , off <- i `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#
      , w <- indexWordArray# a i
      = Ubx.do
         wordWriteAddrLE# w (addr `plusAddr#` off)
         go (i +# 1#)

      | True
      = Ubx.nop

-- | Write a BigNat in base-256 big-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddrBE# :: BigNat# -> Addr# -> State# s -> (# State# s, Word# #)
{-# NOINLINE bigNatToAddrBE# #-}
bigNatToAddrBE# a addr
   | isTrue# (sz ==# 0#) = Ubx.pure 0##
   | msw <- indexWordArray# a (sz -# 1#)
   = Ubx.do
      k <- wordToAddrBE# msw addr
      go (sz -# 1#) (addr `plusAddr#` word2Int# k)
      Ubx.pure (k `plusWord#` (int2Word# (sz -# 1#) `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#))
   where
     sz   = wordArraySize# a

     go i adr
      | 0# <- i
      = Ubx.nop

      | w <- indexWordArray# a (i -# 1#)
      = Ubx.do
         wordWriteAddrBE# w adr
         go (i -# 1#) (adr `plusAddr#` WORD_SIZE_IN_BYTES# )


-- | Write a BigNat in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddr# :: BigNat# -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
bigNatToAddr# a addr 0# s = bigNatToAddrLE# a addr s
bigNatToAddr# a addr _  s = bigNatToAddrBE# a addr s

-- | Write a BigNat in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddr :: BigNat# -> Addr# -> Bool# -> IO Word
bigNatToAddr a addr e = IO Ubx.do
   w <- bigNatToAddr# a addr e
   Ubx.pure (W# w)



-------------------------------------------------
-- Import from memory
-------------------------------------------------

-- | Read a BigNat in base-256 little-endian representation from an Addr#.
--
-- The size is given in bytes.
--
-- Higher limbs equal to 0 are automatically trimmed.
bigNatFromAddrLE# :: Word# -> Addr# -> State# s -> (# State# s, BigNat# #)
{-# NOINLINE bigNatFromAddrLE# #-}
bigNatFromAddrLE# 0## _    = Ubx.pure (bigNatZero# (# #))
bigNatFromAddrLE# sz  addr =
   let
      !nw = sz `uncheckedShiftRL#` WORD_SIZE_BYTES_SHIFT#
      !nb = sz `and#` WORD_SIZE_BYTES_MASK##

      readMSB mwa
         | 0## <- nb
         = Ubx.nop

         | off <- word2Int# (nw `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#)
         = Ubx.do
            w <- wordFromAddrLE# nb (addr `plusAddr#` off)
            mwaWrite# mwa (word2Int# nw) w

      go mwa i
         | isTrue# (i ==# word2Int# nw)
         = Ubx.nop

         | off <- i `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#
         = Ubx.do
            w <- wordFromAddrLE# WORD_SIZE_IN_BYTES## (addr `plusAddr#` off)
            mwaWrite# mwa i w
            go mwa (i +# 1#)

   in Ubx.do
      mwa <- newWordArray# (word2Int# nw +# (word2Int# nb /=# 0#))
      readMSB mwa
      go mwa 0#
      mwaTrimZeroes# mwa
      unsafeFreezeByteArray# mwa

-- | Read a BigNat in base-256 big-endian representation from an Addr#.
--
-- The size is given in bytes.
--
-- Null higher limbs are automatically trimmed.
bigNatFromAddrBE# :: Word# -> Addr# -> State# s -> (# State# s, BigNat# #)
{-# NOINLINE bigNatFromAddrBE# #-}
bigNatFromAddrBE# 0## _    = Ubx.pure (bigNatZero# (# #))
bigNatFromAddrBE# sz  addr =
   let
      !nw = word2Int# (sz `uncheckedShiftRL#` WORD_SIZE_BYTES_SHIFT#)
      !nb = sz `and#` WORD_SIZE_BYTES_MASK##

      goMSB mwa
         | 0## <- nb
         = Ubx.nop

         | True
         = Ubx.do
            w <- wordFromAddrBE# nb addr
            mwaWrite# mwa nw w

      go mwa i
         | isTrue# (i ==# nw)
         = Ubx.nop

         | k <- nw -# 1# -# i
         , off <- (k `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#) +# word2Int# nb
         = Ubx.do
            w <- wordFromAddrBE# WORD_SIZE_IN_BYTES## (addr `plusAddr#` off)
            mwaWrite# mwa i w
            go mwa (i +# 1#)

   in Ubx.do
      mwa <- newWordArray# (nw +# (word2Int# nb /=# 0#))
      goMSB mwa
      go mwa 0#
      mwaTrimZeroes# mwa
      unsafeFreezeByteArray# mwa

-- | Read a BigNat in base-256 representation from an Addr#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimmed.
bigNatFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, BigNat# #)
bigNatFromAddr# sz addr 0# s = bigNatFromAddrLE# sz addr s
bigNatFromAddr# sz addr _  s = bigNatFromAddrBE# sz addr s

-------------------------------------------------
-- Export to ByteArray
-------------------------------------------------

-- | Write a BigNat in base-256 little-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArrayLE# :: BigNat# -> MutableByteArray# s -> Word# -> State# s -> (# State# s, Word# #)
{-# NOINLINE bigNatToMutableByteArrayLE# #-}
bigNatToMutableByteArrayLE# a mba moff
   | isTrue# (sz ==# 0#) = Ubx.pure 0##
   | True = Ubx.do
      k <- writeMSB
      go 0#
      Ubx.pure (k `plusWord#` (int2Word# li `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#))
   where
     !sz = wordArraySize# a
     !li = sz -# 1#

     writeMSB = wordToMutableByteArrayLE# (indexWordArray# a li)
                  mba (moff `plusWord#` int2Word# (li `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#))

     go i
      | isTrue# (i <# li)
      , off <- int2Word# i `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#
      , w <- indexWordArray# a i
      = Ubx.do
         wordWriteMutableByteArrayLE# w mba (moff `plusWord#` off)
         go (i +# 1#)

      | True
      = Ubx.nop

-- | Write a BigNat in base-256 big-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArrayBE# :: BigNat# -> MutableByteArray# s -> Word# -> State# s -> (# State# s, Word# #)
{-# NOINLINE bigNatToMutableByteArrayBE# #-}
bigNatToMutableByteArrayBE# a mba moff
   | isTrue# (sz ==# 0#) = Ubx.pure 0##
   | msw <- indexWordArray# a (sz -# 1#)
   = Ubx.do
      k <- wordToMutableByteArrayBE# msw mba moff
      go (sz -# 1#) k
      Ubx.pure (k `plusWord#` (int2Word# (sz -# 1#) `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#))
   where
     sz   = wordArraySize# a

     go i c
      | 0# <- i
      = Ubx.nop

      | w <- indexWordArray# a (i -# 1#)
      = Ubx.do
         wordWriteMutableByteArrayBE# w mba (moff `plusWord#` c)
         go (i -# 1#) (c `plusWord#` WORD_SIZE_IN_BYTES## )


-- | Write a BigNat in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArray# :: BigNat# -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
bigNatToMutableByteArray# a mba off 0# s = bigNatToMutableByteArrayLE# a mba off s
bigNatToMutableByteArray# a mba off _  s = bigNatToMutableByteArrayBE# a mba off s

-------------------------------------------------
-- Import from ByteArray
-------------------------------------------------

-- | Read a BigNat in base-256 little-endian representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- Null higher limbs are automatically trimmed.
bigNatFromByteArrayLE# :: Word# -> ByteArray# -> Word# -> State# s -> (# State# s, BigNat# #)
{-# NOINLINE bigNatFromByteArrayLE# #-}
bigNatFromByteArrayLE# 0## _  _    = Ubx.pure (bigNatZero# (# #))
bigNatFromByteArrayLE# sz  ba moff =
   let
      !nw = sz `uncheckedShiftRL#` WORD_SIZE_BYTES_SHIFT#
      !nb = sz `and#` WORD_SIZE_BYTES_MASK##

      readMSB mwa
         | 0## <- nb
         = Ubx.nop

         | off <- nw `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#
         = case wordFromByteArrayLE# nb ba (moff `plusWord#` off) of
               w -> mwaWrite# mwa (word2Int# nw) w

      go mwa i
         | isTrue# (i `eqWord#` nw)
         = Ubx.nop

         | off <- i `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#
         = case wordFromByteArrayLE# WORD_SIZE_IN_BYTES## ba (moff `plusWord#` off) of
               w -> Ubx.do
                  mwaWrite# mwa (word2Int# i) w
                  go mwa (i `plusWord#` 1##)

   in Ubx.do
      mwa <- newWordArray# (word2Int# nw +# (word2Int# nb /=# 0#))
      readMSB mwa
      go mwa 0##
      mwaTrimZeroes# mwa
      unsafeFreezeByteArray# mwa

-- | Read a BigNat in base-256 big-endian representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- Null higher limbs are automatically trimmed.
bigNatFromByteArrayBE# :: Word# -> ByteArray# -> Word# -> State# s -> (# State# s, BigNat# #)
{-# NOINLINE bigNatFromByteArrayBE# #-}
bigNatFromByteArrayBE# 0## _  _    = Ubx.pure (bigNatZero# (# #))
bigNatFromByteArrayBE# sz  ba moff =
   let
      !nw = sz `uncheckedShiftRL#` WORD_SIZE_BYTES_SHIFT#
      !nb = sz `and#` WORD_SIZE_BYTES_MASK##

      goMSB mwa
         | 0## <- nb
         = Ubx.nop

         | True
         = case wordFromByteArrayBE# nb ba moff of
            w -> mwaWrite# mwa (word2Int# nw) w

      go mwa i
         | isTrue# (i `eqWord#` nw)
         = Ubx.nop

         | k <- nw `minusWord#` 1## `minusWord#` i
         , off <- (k `uncheckedShiftL#` WORD_SIZE_BYTES_SHIFT#) `plusWord#` nb
         = case wordFromByteArrayBE# WORD_SIZE_IN_BYTES## ba (moff `plusWord#` off) of
            w -> Ubx.do
               mwaWrite# mwa (word2Int# i) w
               go mwa (i `plusWord#` 1##)

   in Ubx.do
      mwa <- newWordArray# (word2Int# nw +# (word2Int# nb /=# 0#))
      goMSB mwa
      go mwa 0##
      mwaTrimZeroes# mwa
      unsafeFreezeByteArray# mwa

-- | Read a BigNat in base-256 representation from a ByteArray#.
--
-- The size is given in bytes.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimmed.
bigNatFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> State# s -> (# State# s, BigNat# #)
bigNatFromByteArray# sz ba off 0# s = bigNatFromByteArrayLE# sz ba off s
bigNatFromByteArray# sz ba off _  s = bigNatFromByteArrayBE# sz ba off s




-- | Create a BigNat# from a WordArray# containing /n/ limbs in
-- least-significant-first order.
--
-- If possible 'WordArray#', will be used directly (i.e. shared
-- /without/ cloning the 'WordArray#' into a newly allocated one)
bigNatFromWordArray# :: WordArray# -> Word# -> BigNat#
{-# NOINLINE bigNatFromWordArray# #-}
bigNatFromWordArray# wa n0
   | isTrue# (n `eqWord#` 0##)
   = bigNatZero# (# #)

   | isTrue# (r `eqWord#` 0##) -- i.e. wa is multiple of limb-size
   , isTrue# (q `eqWord#` n)
   = wa

   | True = withNewWordArray# (word2Int# n) \mwa s ->
               mwaArrayCopy# mwa 0# wa 0# (word2Int# n) s
   where
      !(# q, r #) = quotRemWord# (int2Word# (sizeofByteArray# wa))
                                 WORD_SIZE_IN_BYTES##
      -- find real size in Words by removing trailing null limbs
      !n = real_size n0
      real_size 0## = 0##
      real_size i
           | 0## <- bigNatIndex# wa (word2Int# (i `minusWord#` 1##))
           = real_size (i `minusWord#` 1##)
      real_size i = i


-- | Create a BigNat from a WordArray# containing /n/ limbs in
-- least-significant-first order.
--
-- If possible 'WordArray#', will be used directly (i.e. shared
-- /without/ cloning the 'WordArray#' into a newly allocated one)
bigNatFromWordArray :: WordArray# -> Word# -> BigNat
bigNatFromWordArray wa n = BN# (bigNatFromWordArray# wa n)

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Eq BigNat where
   BN# a == BN# b = bigNatEq a b
   BN# a /= BN# b = bigNatNe a b

instance Ord BigNat where
   (BN# a) `compare` (BN# b) = bigNatCompare a b
   BN# a <  BN# b = bigNatLt a b
   BN# a <= BN# b = bigNatLe a b
   BN# a >  BN# b = bigNatGt a b
   BN# a >= BN# b = bigNatGe a b
