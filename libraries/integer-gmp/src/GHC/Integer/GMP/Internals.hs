{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

#include "MachDeps.h"

-- |
-- Module      :  GHC.Integer.GMP.Internals
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
module GHC.Integer.GMP.Internals
    ( -- * The 'Integer' type
      Integer (S#,Jn#,Jp#)
    , isValidInteger#

      -- ** Basic 'Integer' operations

    , module GHC.Internal.Integer

      -- ** Additional 'Integer' operations
    , gcdInteger
    , gcdExtInteger
    , lcmInteger
    , sqrInteger
    , powModInteger
    , recipModInteger

      -- ** Additional conversion operations to 'Integer'
    , wordToNegInteger
    , bigNatToInteger
    , bigNatToNegInteger

      -- * The 'BigNat' type
    , BigNat(..)

    , GmpLimb, GmpLimb#
    , GmpSize, GmpSize#

      -- **

    , isValidBigNat#
    , sizeofBigNat#
    , zeroBigNat
    , oneBigNat

      -- ** Conversions to/from 'BigNat'

    , byteArrayToBigNat#
    , wordToBigNat
    , wordToBigNat2
    , bigNatToInt
    , bigNatToWord
    , indexBigNat#

      -- ** 'BigNat' arithmetic operations
    , plusBigNat
    , plusBigNatWord
    , minusBigNat
    , minusBigNatWord
    , timesBigNat
    , timesBigNatWord
    , sqrBigNat

    , quotRemBigNat
    , quotRemBigNatWord
    , quotBigNatWord
    , quotBigNat
    , remBigNat
    , remBigNatWord

    , gcdBigNat
    , gcdBigNatWord

      -- ** 'BigNat' logic operations
    , shiftRBigNat
    , shiftLBigNat
    , testBitBigNat
    , clearBitBigNat
    , complementBitBigNat
    , setBitBigNat
    , andBigNat
    , xorBigNat
    , popCountBigNat
    , orBigNat
    , bitBigNat

      -- ** 'BigNat' comparison predicates
    , isZeroBigNat

    , compareBigNatWord
    , compareBigNat
    , eqBigNatWord
    , eqBigNatWord#
    , eqBigNat
    , eqBigNat#
    , gtBigNatWord#

      -- * Import/export functions
      -- ** Compute size of serialisation
    , sizeInBaseBigNat
    , sizeInBaseInteger
    , sizeInBaseWord#

      -- ** Export
    , exportBigNatToAddr
    , exportIntegerToAddr

    , exportBigNatToMutableByteArray
    , exportIntegerToMutableByteArray

      -- ** Import
    , importBigNatFromAddr
    , importIntegerFromAddr

    , importBigNatFromByteArray
    , importIntegerFromByteArray
    ) where

import GHC.Internal.Integer
import GHC.Internal.Natural
import GHC.Internal.Bignum.Integer (Integer(..))
import qualified GHC.Internal.Bignum.Integer as I
import qualified GHC.Internal.Bignum.BigNat as B
import qualified GHC.Internal.Bignum.Primitives as P
import GHC.Types
import GHC.Prim
import GHC.Exts (runRW#)
import Control.Exception

{-# COMPLETE S#, Jp#, Jn# #-}

{-# DEPRECATED S# "Use IS constructor instead" #-}
pattern S# :: Int# -> Integer
pattern S# i = IS i

fromBN# :: BigNat -> ByteArray#
fromBN# (BN# x) = x

fromIP :: Integer -> (# () | BigNat #)
fromIP (IP x) = (# | BN# x #)
fromIP _      = (# () | #)

fromIN :: Integer -> (# () | BigNat #)
fromIN (IN x) = (# | BN# x #)
fromIN _      = (# () | #)

{-# DEPRECATED Jp# "Use IP constructor instead" #-}
pattern Jp# :: BigNat -> Integer
pattern Jp# i <- (fromIP -> (# | i #))
   where
      Jp# i = IP (fromBN# i)

{-# DEPRECATED Jn# "Use IN constructor instead" #-}
pattern Jn# :: BigNat -> Integer
pattern Jn# i <- (fromIN -> (# | i #))
   where
      Jn# i = IN (fromBN# i)

{-# DEPRECATED isValidInteger# "Use integerCheck# instead" #-}
isValidInteger# :: Integer -> Int#
isValidInteger# = I.integerCheck#

{-# DEPRECATED gcdInteger "Use integerGcd instead" #-}
gcdInteger :: Integer -> Integer -> Integer
gcdInteger = I.integerGcd

{-# DEPRECATED gcdExtInteger "Use integerGcde instead" #-}
gcdExtInteger :: Integer -> Integer -> (# Integer, Integer #)
gcdExtInteger a b = case I.integerGcde# a b of
   (# g, s, _t #) -> (# g, s #)


{-# DEPRECATED lcmInteger "Use integerLcm instead" #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger = I.integerLcm

{-# DEPRECATED sqrInteger "Use integerSqr instead" #-}
sqrInteger :: Integer -> Integer
sqrInteger = I.integerSqr

{-# DEPRECATED recipModInteger "Use integerRecipMod# instead" #-}
recipModInteger :: Integer -> Integer -> Integer
recipModInteger x m = case I.integerRecipMod# x (I.integerToNatural m) of
   (# y |    #) -> I.integerFromNatural y
   (#   | () #) -> 0

{-# DEPRECATED powModInteger "Use integerPowMod# instead" #-}
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger b e m = case I.integerPowMod# b e (I.integerToNatural m) of
   (# r  | #) -> I.integerFromNatural r
   (# | () #) -> 0

{-# DEPRECATED wordToNegInteger "Use integerFromWordNeg# instead" #-}
wordToNegInteger :: Word# -> Integer
wordToNegInteger = I.integerFromWordNeg#

{-# DEPRECATED bigNatToInteger "Use integerFromBigNat# instead" #-}
bigNatToInteger :: BigNat -> Integer
bigNatToInteger (BN# i) = I.integerFromBigNat# i

{-# DEPRECATED bigNatToNegInteger "Use integerFromBigNatNeg# instead" #-}
bigNatToNegInteger :: BigNat -> Integer
bigNatToNegInteger (BN# i) = I.integerFromBigNatNeg# i

type GmpLimb = Word
type GmpLimb# = Word#
type GmpSize = Int
type GmpSize# = Int#

{-# DEPRECATED sizeofBigNat# "Use bigNatSize# instead" #-}
sizeofBigNat# :: BigNat -> GmpSize#
sizeofBigNat# (BN# i) = B.bigNatSize# i

{-# DEPRECATED isValidBigNat# "Use bigNatCheck# instead" #-}
isValidBigNat# :: BigNat -> Int#
isValidBigNat# (BN# i) = B.bigNatCheck# i

{-# DEPRECATED zeroBigNat "Use bigNatZero instead" #-}
zeroBigNat :: BigNat
zeroBigNat = B.bigNatZero

{-# DEPRECATED oneBigNat "Use bigNatOne instead" #-}
oneBigNat :: BigNat
oneBigNat = B.bigNatOne

{-# DEPRECATED plusBigNat "Use bigNatAdd instead" #-}
plusBigNat :: BigNat -> BigNat -> BigNat
plusBigNat (BN# a) (BN# b) = BN# (B.bigNatAdd a b)

{-# DEPRECATED plusBigNatWord "Use bigNatAddWord# instead" #-}
plusBigNatWord :: BigNat -> GmpLimb# -> BigNat
plusBigNatWord (BN# a) w = BN# (B.bigNatAddWord# a w)

{-# DEPRECATED minusBigNat "Use bigNatSub instead" #-}
minusBigNat :: BigNat -> BigNat -> BigNat
minusBigNat (BN# a) (BN# b) = case B.bigNatSub a b of
   (# (# #) | #)   -> throw Underflow
   (#       | r #) -> BN# r

{-# DEPRECATED minusBigNatWord "Use bigNatSubWord# instead" #-}
minusBigNatWord :: BigNat -> GmpLimb# -> BigNat
minusBigNatWord (BN# a) b = case B.bigNatSubWord# a b of
   (# (# #) | #)   -> throw Underflow
   (#       | r #) -> BN# r


{-# DEPRECATED timesBigNat "Use bigNatMul instead" #-}
timesBigNat :: BigNat -> BigNat -> BigNat
timesBigNat (BN# a) (BN# b) = BN# (B.bigNatMul a b)

{-# DEPRECATED timesBigNatWord "Use bigNatMulWord# instead" #-}
timesBigNatWord :: BigNat -> GmpLimb# -> BigNat
timesBigNatWord (BN# a) w = BN# (B.bigNatMulWord# a w)

{-# DEPRECATED sqrBigNat "Use bigNatSqr instead" #-}
sqrBigNat :: BigNat -> BigNat
sqrBigNat (BN# a) = BN# (B.bigNatSqr a)

{-# DEPRECATED quotRemBigNat "Use bigNatQuotRem# instead" #-}
quotRemBigNat :: BigNat -> BigNat -> (# BigNat,BigNat #)
quotRemBigNat (BN# a) (BN# b) = case B.bigNatQuotRem# a b of
   (# q, r #) -> (# BN# q, BN# r #)

{-# DEPRECATED quotRemBigNatWord "Use bigNatQuotRemWord# instead" #-}
quotRemBigNatWord :: BigNat -> GmpLimb# -> (# BigNat, GmpLimb# #)
quotRemBigNatWord (BN# a) b = case B.bigNatQuotRemWord# a b of
   (# q, r #) -> (# BN# q, r #)

{-# DEPRECATED quotBigNat "Use bigNatQuot instead" #-}
quotBigNat :: BigNat -> BigNat -> BigNat
quotBigNat (BN# a) (BN# b) = BN# (B.bigNatQuot a b)

{-# DEPRECATED quotBigNatWord "Use bigNatQuotWord# instead" #-}
quotBigNatWord :: BigNat -> GmpLimb# -> BigNat
quotBigNatWord (BN# a) b = BN# (B.bigNatQuotWord# a b)

{-# DEPRECATED remBigNat "Use bigNatRem instead" #-}
remBigNat :: BigNat -> BigNat -> BigNat
remBigNat (BN# a) (BN# b) = BN# (B.bigNatRem a b)

{-# DEPRECATED remBigNatWord "Use bigNatRemWord# instead" #-}
remBigNatWord :: BigNat -> GmpLimb# -> Word#
remBigNatWord (BN# a) b = B.bigNatRemWord# a b

{-# DEPRECATED gcdBigNatWord "Use bigNatGcdWord# instead" #-}
gcdBigNatWord :: BigNat -> Word# -> Word#
gcdBigNatWord (BN# a) b = B.bigNatGcdWord# a b

{-# DEPRECATED gcdBigNat "Use bigNatGcd instead" #-}
gcdBigNat:: BigNat -> BigNat -> BigNat
gcdBigNat (BN# a) (BN# b) = BN# (B.bigNatGcd a b)

{-# DEPRECATED shiftRBigNat "Use bigNatShiftR# instead" #-}
shiftRBigNat :: BigNat -> Int# -> BigNat
shiftRBigNat (BN# a) i = BN# (B.bigNatShiftR# a (int2Word# i))

{-# DEPRECATED shiftLBigNat "Use bigNatShiftL# instead" #-}
shiftLBigNat :: BigNat -> Int# -> BigNat
shiftLBigNat (BN# a) i = BN# (B.bigNatShiftL# a (int2Word# i))

{-# DEPRECATED testBitBigNat "Use bigNatTestBit# instead" #-}
testBitBigNat :: BigNat -> Int# -> Bool
testBitBigNat (BN# a) i = isTrue# (B.bigNatTestBit# a (int2Word# i))

{-# DEPRECATED clearBitBigNat "Use bigNatClearBit# instead" #-}
clearBitBigNat :: BigNat -> Int# -> BigNat
clearBitBigNat (BN# a) i = BN# (B.bigNatClearBit# a (int2Word# i))

{-# DEPRECATED complementBitBigNat "Use bigNatComplementBit# instead" #-}
complementBitBigNat :: BigNat -> Int# -> BigNat
complementBitBigNat (BN# a) i = BN# (B.bigNatComplementBit# a (int2Word# i))

{-# DEPRECATED setBitBigNat "Use bigNatSetBit# instead" #-}
setBitBigNat :: BigNat -> Int# -> BigNat
setBitBigNat (BN# a) i = BN# (B.bigNatSetBit# a (int2Word# i))

{-# DEPRECATED andBigNat "Use bigNatAnd instead" #-}
andBigNat :: BigNat -> BigNat -> BigNat
andBigNat (BN# a) (BN# b) = BN# (B.bigNatAnd a b)

{-# DEPRECATED orBigNat "Use bigNatOr instead" #-}
orBigNat :: BigNat -> BigNat -> BigNat
orBigNat (BN# a) (BN# b) = BN# (B.bigNatOr a b)

{-# DEPRECATED xorBigNat "Use bigNatXor instead" #-}
xorBigNat :: BigNat -> BigNat -> BigNat
xorBigNat (BN# a) (BN# b) = BN# (B.bigNatXor a b)

{-# DEPRECATED popCountBigNat "Use bigNatPopCount# instead" #-}
popCountBigNat :: BigNat -> Int#
popCountBigNat (BN# a) = word2Int# (B.bigNatPopCount# a)

{-# DEPRECATED bitBigNat "Use bigNatBit# instead" #-}
bitBigNat :: Int# -> BigNat
bitBigNat i = BN# (B.bigNatBit# (int2Word# i))

{-# DEPRECATED isZeroBigNat "Use bigNatIsZero instead" #-}
isZeroBigNat :: BigNat -> Bool
isZeroBigNat (BN# a) = B.bigNatIsZero a

{-# DEPRECATED compareBigNat "Use bigNatCompare instead" #-}
compareBigNat :: BigNat -> BigNat -> Ordering
compareBigNat (BN# a) (BN# b) = B.bigNatCompare a b

{-# DEPRECATED compareBigNatWord "Use bigNatCompareWord# instead" #-}
compareBigNatWord :: BigNat -> GmpLimb# -> Ordering
compareBigNatWord (BN# a) w = B.bigNatCompareWord# a w

{-# DEPRECATED eqBigNatWord "Use bigNatEqWord# instead" #-}
eqBigNatWord :: BigNat -> GmpLimb# -> Bool
eqBigNatWord (BN# a) w = isTrue# (B.bigNatEqWord# a w)

{-# DEPRECATED eqBigNatWord# "Use bigNatEqWord# instead" #-}
eqBigNatWord# :: BigNat -> GmpLimb# -> Int#
eqBigNatWord# (BN# a) w = B.bigNatEqWord# a w

{-# DEPRECATED eqBigNat# "Use bigNatEq# instead" #-}
eqBigNat# :: BigNat -> BigNat -> Int#
eqBigNat# (BN# a) (BN# b) = B.bigNatEq# a b

{-# DEPRECATED eqBigNat "Use bigNatEq instead" #-}
eqBigNat :: BigNat -> BigNat -> Bool
eqBigNat (BN# a) (BN# b) = B.bigNatEq a b

{-# DEPRECATED gtBigNatWord# "Use bigNatGtWord# instead" #-}
gtBigNatWord# :: BigNat -> GmpLimb# -> Int#
gtBigNatWord# (BN# a) w = B.bigNatGtWord# a w

{-# DEPRECATED sizeInBaseBigNat "Use bigNatSizeInBase# instead" #-}
sizeInBaseBigNat :: BigNat -> Int# -> Word#
sizeInBaseBigNat (BN# a) b = B.bigNatSizeInBase# (int2Word# b) a

{-# DEPRECATED sizeInBaseInteger "Use integerSizeInBase# instead" #-}
sizeInBaseInteger :: Integer -> Int# -> Word#
sizeInBaseInteger i b = I.integerSizeInBase# (int2Word# b) i

{-# DEPRECATED sizeInBaseWord# "Use wordSizeInBase# instead" #-}
sizeInBaseWord# :: Word# -> Int# -> Word#
sizeInBaseWord# a b = P.wordSizeInBase# (int2Word# b) a

{-# DEPRECATED importBigNatFromAddr "Use bigNatFromAddr# instead" #-}
importBigNatFromAddr :: Addr# -> Word# -> Int# -> IO BigNat
importBigNatFromAddr addr sz endian = IO \s ->
   case B.bigNatFromAddr# sz addr endian s of
      (# s', b #) -> (# s', BN# b #)

{-# DEPRECATED exportBigNatToAddr "Use bigNatToAddr# instead" #-}
exportBigNatToAddr :: BigNat -> Addr# -> Int# -> IO Word
exportBigNatToAddr (BN# b) addr endian = IO \s ->
   case B.bigNatToAddr# b addr endian s of
      (# s', w #) -> (# s', W# w #)

{-# DEPRECATED importIntegerFromAddr "Use integerFromAddr# instead" #-}
importIntegerFromAddr :: Addr# -> Word# -> Int# -> IO Integer
importIntegerFromAddr addr sz endian = IO \s ->
   case I.integerFromAddr# sz addr endian s of
      (# s', i #) -> (# s', i #)

{-# DEPRECATED exportIntegerToAddr "Use integerToAddr# instead" #-}
exportIntegerToAddr :: Integer -> Addr# -> Int# -> IO Word
exportIntegerToAddr i addr endian = IO \s ->
   case I.integerToAddr# i addr endian s of
      (# s', w #) -> (# s', W# w #)

wordToBigNat :: Word# -> BigNat
wordToBigNat w = BN# (B.bigNatFromWord# w)

wordToBigNat2 :: Word# -> Word# -> BigNat
wordToBigNat2 h l = BN# (B.bigNatFromWord2# h l)

bigNatToInt :: BigNat -> Int#
bigNatToInt (BN# b) = B.bigNatToInt# b

bigNatToWord :: BigNat -> Word#
bigNatToWord (BN# b) = B.bigNatToWord# b

{-# DEPRECATED indexBigNat# "Use bigNatIndex# instead" #-}
indexBigNat# :: BigNat -> GmpSize# -> GmpLimb#
indexBigNat# (BN# b) i = B.bigNatIndex# b i

{-# DEPRECATED importBigNatFromByteArray "Use bigNatFromByteArray# instead" #-}
importBigNatFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> BigNat
importBigNatFromByteArray ba off sz endian = case runRW# (B.bigNatFromByteArray# sz ba off endian) of
   (# _, r #) -> BN# r

{-# DEPRECATED exportBigNatToMutableByteArray "Use bigNatToMutableByteArray# instead" #-}
exportBigNatToMutableByteArray :: BigNat -> MutableByteArray# RealWorld -> Word# -> Int# -> IO Word
exportBigNatToMutableByteArray (BN# ba) mba off endian = IO (\s -> case B.bigNatToMutableByteArray# ba mba off endian s of
   (# s', r #) -> (# s', W# r #))

{-# DEPRECATED importIntegerFromByteArray "Use integerFromByteArray# instead" #-}
importIntegerFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> Integer
importIntegerFromByteArray ba off sz endian = case runRW# (I.integerFromByteArray# sz ba off endian) of
   (# _, r #) -> r

{-# DEPRECATED exportIntegerToMutableByteArray "Use integerToMutableByteArray# instead" #-}
exportIntegerToMutableByteArray :: Integer -> MutableByteArray# RealWorld -> Word# -> Int# -> IO Word
exportIntegerToMutableByteArray i mba off endian = IO (\s -> case I.integerToMutableByteArray# i mba off endian s of
   (# s', r #) -> (# s', W# r #))


{-# DEPRECATED byteArrayToBigNat# "Use bigNatFromWordArray instead" #-}
byteArrayToBigNat# :: ByteArray# -> GmpSize# -> BigNat
byteArrayToBigNat# ba n = B.bigNatFromWordArray ba (int2Word# n)
