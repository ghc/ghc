{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Compatibility module for pre ghc-bignum code.
module GHC.Natural
   ( Natural (NatS#, NatJ#)
   , B.BigNat (..)
   , mkNatural
   , isValidNatural
     -- * Arithmetic
   , plusNatural
   , minusNatural
   , minusNaturalMaybe
   , timesNatural
   , negateNatural
   , signumNatural
   , quotRemNatural
   , quotNatural
   , remNatural
   , gcdNatural
   , lcmNatural
     -- * Bits
   , andNatural
   , orNatural
   , xorNatural
   , bitNatural
   , testBitNatural
   , popCountNatural
   , shiftLNatural
   , shiftRNatural
     -- * Conversions
   , naturalToInteger
   , naturalToWord
   , naturalToWordMaybe
   , wordToNatural
   , wordToNatural#
   , naturalFromInteger
     -- * Modular arithmetic
   , powModNatural
   )
where

import GHC.Prim
import GHC.Types
import GHC.Maybe
import GHC.Num.Natural (Natural)
import GHC.Num.Integer (Integer)
import qualified GHC.Num.BigNat  as B
import qualified GHC.Num.Natural as N
import qualified GHC.Num.Integer as I

{-# COMPLETE NatS#, NatJ# #-}

pattern NatS# :: Word# -> Natural
pattern NatS# w = N.NS w

pattern NatJ# :: B.BigNat -> Natural
pattern NatJ# b <- N.NB (B.BN# -> b)
   where
      NatJ# b = N.NB (B.unBigNat b)

int2Word :: Int -> Word
int2Word (I# i) = W# (int2Word# i)

word2Int :: Word -> Int
word2Int (W# w) = I# (word2Int# w)

-- | Construct 'Natural' value from list of 'Word's.
mkNatural :: [Word] -> Natural
mkNatural = N.naturalFromWordList

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural = N.naturalCheck

-- | 'Natural' Addition
plusNatural :: Natural -> Natural -> Natural
plusNatural = N.naturalAdd

-- | 'Natural' subtraction. May @'Control.Exception.throw'
-- 'Control.Exception.Underflow'@.
minusNatural :: Natural -> Natural -> Natural
minusNatural = N.naturalSubThrow

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x y = case N.naturalSub x y of
   (# (# #) |   #) -> Nothing
   (#       | n #) -> Just n

-- | 'Natural' multiplication
timesNatural :: Natural -> Natural -> Natural
timesNatural = N.naturalMul

negateNatural :: Natural -> Natural
negateNatural = N.naturalNegate

signumNatural :: Natural -> Natural
signumNatural = N.naturalSignum

quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural = N.naturalQuotRem

remNatural :: Natural -> Natural -> Natural
remNatural = N.naturalRem

quotNatural :: Natural -> Natural -> Natural
quotNatural = N.naturalQuot

-- | Compute greatest common divisor.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural = N.naturalGcd

-- | Compute least common multiple.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural = N.naturalLcm

andNatural :: Natural -> Natural -> Natural
andNatural = N.naturalAnd

orNatural :: Natural -> Natural -> Natural
orNatural = N.naturalOr

xorNatural :: Natural -> Natural -> Natural
xorNatural = N.naturalXor

bitNatural :: Int# -> Natural
bitNatural i = N.naturalBit# (int2Word# i)

testBitNatural :: Natural -> Int -> Bool
testBitNatural n i = N.naturalTestBit n (int2Word i)

popCountNatural :: Natural -> Int
popCountNatural n = word2Int (N.naturalPopCount n)

shiftLNatural :: Natural -> Int -> Natural
shiftLNatural n i = N.naturalShiftL n (int2Word  i)

shiftRNatural :: Natural -> Int -> Natural
shiftRNatural n i = N.naturalShiftR n (int2Word i)

-- | @since 4.12.0.0
naturalToInteger :: Natural -> Integer
naturalToInteger = I.integerFromNatural

naturalToWord :: Natural -> Word
naturalToWord = N.naturalToWord

-- | @since 4.10.0.0
naturalFromInteger :: Integer -> Natural
naturalFromInteger = I.integerToNatural

-- | Construct 'Natural' from 'Word' value.
--
-- @since 4.8.0.0
wordToNatural :: Word -> Natural
wordToNatural = N.naturalFromWord

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns 'Nothing' if value doesn't fit in 'Word'.
--
-- @since 4.8.0.0
naturalToWordMaybe :: Natural -> Maybe Word
naturalToWordMaybe n = case N.naturalToWordMaybe# n of
   (#       | w #) -> Just (W# w)
   (# (# #) |   #) -> Nothing

wordToNatural# :: Word -> Natural
wordToNatural# = N.naturalFromWord

-- | \"@'powModNatural' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- @since 4.8.0.0
powModNatural :: Natural -> Natural -> Natural -> Natural
powModNatural = N.naturalPowMod
