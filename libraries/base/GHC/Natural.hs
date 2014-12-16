{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (C) 2014 Herbert Valerio Riedel,
--                (C) 2011 Edward Kmett
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The arbitrary-precision 'Natural' number type.
--
-- __Note__: This is an internal GHC module with an API subject to
-- change.  It's recommended use the "Numeric.Natural" module to import
-- the 'Natural' type.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------
module GHC.Natural
    ( -- * The 'Natural' number type
      --
      -- | __Warning__: The internal implementation of 'Natural'
      -- (i.e. which constructors are available) depends on the
      -- 'Integer' backend used!
      Natural(..)
    , isValidNatural
      -- * Conversions
    , wordToNatural
    , naturalToWordMaybe
      -- * Checked subtraction
    , minusNaturalMaybe
      -- * Modular arithmetic
    , powModNatural
    ) where

#include "MachDeps.h"

#if defined(MIN_VERSION_integer_gmp)
# define HAVE_GMP_BIGNAT MIN_VERSION_integer_gmp(1,0,0)
#else
# define HAVE_GMP_BIGNAT 0
#endif

import GHC.Arr
import GHC.Base
import GHC.Exception
#if HAVE_GMP_BIGNAT
import GHC.Integer.GMP.Internals
import Data.Word
import Data.Int
#endif
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Show
import GHC.Enum
import GHC.List

import Data.Bits
import Data.Data

default ()

#if HAVE_GMP_BIGNAT
-- TODO: if saturated arithmetic is to used, replace 'throw Underflow' by '0'

-- | Type representing arbitrary-precision non-negative integers.
--
-- Operations whose result would be negative
-- @'throw' ('Underflow' :: 'ArithException')@.
--
-- @since 4.8.0.0
data Natural = NatS#                 GmpLimb# -- ^ in @[0, maxBound::Word]@
             | NatJ# {-# UNPACK #-} !BigNat   -- ^ in @]maxBound::Word, +inf[@
                                              --
                                              -- __Invariant__: 'NatJ#' is used
                                              -- /iff/ value doesn't fit in
                                              -- 'NatS#' constructor.
             deriving (Eq,Ord) -- NB: Order of constructors *must*
                               -- coincide with 'Ord' relation

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (NatS# _)  = True
isValidNatural (NatJ# bn) = isTrue# (isValidBigNat# bn)
                            && I# (sizeofBigNat# bn) > 0

{-# RULES
"fromIntegral/Natural->Natural"  fromIntegral = id :: Natural -> Natural
"fromIntegral/Natural->Integer"  fromIntegral = toInteger :: Natural->Integer
"fromIntegral/Natural->Word"     fromIntegral = naturalToWord
"fromIntegral/Natural->Word8"
    fromIntegral = (fromIntegral :: Word -> Word8)  . naturalToWord
"fromIntegral/Natural->Word16"
    fromIntegral = (fromIntegral :: Word -> Word16) . naturalToWord
"fromIntegral/Natural->Word32"
    fromIntegral = (fromIntegral :: Word -> Word32) . naturalToWord
"fromIntegral/Natural->Int8"
    fromIntegral = (fromIntegral :: Int -> Int8)    . naturalToInt
"fromIntegral/Natural->Int16"
    fromIntegral = (fromIntegral :: Int -> Int16)   . naturalToInt
"fromIntegral/Natural->Int32"
    fromIntegral = (fromIntegral :: Int -> Int32)   . naturalToInt
  #-}

{-# RULES
"fromIntegral/Word->Natural"     fromIntegral = wordToNatural
"fromIntegral/Word8->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word8  -> Word)
"fromIntegral/Word16->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word16 -> Word)
"fromIntegral/Word32->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word32 -> Word)
"fromIntegral/Int->Natural"     fromIntegral = intToNatural
"fromIntegral/Int8->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int8  -> Int)
"fromIntegral/Int16->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int16 -> Int)
"fromIntegral/Int32->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int32 -> Int)
  #-}

#if WORD_SIZE_IN_BITS == 64
-- these RULES are valid for Word==Word64 & Int==Int64
{-# RULES
"fromIntegral/Natural->Word64"
    fromIntegral = (fromIntegral :: Word -> Word64) . naturalToWord
"fromIntegral/Natural->Int64"
    fromIntegral = (fromIntegral :: Int -> Int64) . naturalToInt
"fromIntegral/Word64->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word64 -> Word)
"fromIntegral/Int64->Natural"
    fromIntegral = intToNatural . (fromIntegral :: Int64 -> Int)
  #-}
#endif

instance Show Natural where
    showsPrec p (NatS# w#)  = showsPrec p (W# w#)
    showsPrec p (NatJ# bn)  = showsPrec p (Jp# bn)

instance Read Natural where
    readsPrec d = map (\(n, s) -> (fromInteger n, s))
                  . filter ((>= 0) . (\(x,_)->x)) . readsPrec d

instance Num Natural where
    fromInteger (S# i#) | I# i# >= 0  = NatS# (int2Word# i#)
    fromInteger (Jp# bn)              = bigNatToNatural bn
    fromInteger _                     = throw Underflow

    (+) = plusNatural
    (*) = timesNatural
    (-) = minusNatural

    abs                  = id

    signum (NatS# 0##)   = NatS# 0##
    signum _             = NatS# 1##

    negate (NatS# 0##)   = NatS# 0##
    negate _             = throw Underflow

instance Real Natural where
    toRational (NatS# w)  = toRational (W# w)
    toRational (NatJ# bn) = toRational (Jp# bn)

#if OPTIMISE_INTEGER_GCD_LCM
{-# RULES
"gcd/Natural->Natural->Natural" gcd = gcdNatural
"lcm/Natural->Natural->Natural" lcm = lcmNatural
  #-}

-- | Compute greatest common divisor.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural (NatS# 0##) y       = y
gcdNatural x       (NatS# 0##) = x
gcdNatural (NatS# 1##) _       = (NatS# 1##)
gcdNatural _       (NatS# 1##) = (NatS# 1##)
gcdNatural (NatJ# x) (NatJ# y) = bigNatToNatural (gcdBigNat x y)
gcdNatural (NatJ# x) (NatS# y) = NatS# (gcdBigNatWord x y)
gcdNatural (NatS# x) (NatJ# y) = NatS# (gcdBigNatWord y x)
gcdNatural (NatS# x) (NatS# y) = NatS# (gcdWord x y)

-- | compute least common multiplier.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural (NatS# 0##) _ = (NatS# 0##)
lcmNatural _ (NatS# 0##) = (NatS# 0##)
lcmNatural (NatS# 1##) y = y
lcmNatural x (NatS# 1##) = x
lcmNatural x y           = (x `quot` (gcdNatural x y)) * y

#endif

instance Enum Natural where
    succ n = n `plusNatural`  NatS# 1##
    pred n = n `minusNatural` NatS# 1##

    toEnum = intToNatural

    fromEnum (NatS# w) | i >= 0 = i
      where
        i = fromIntegral (W# w)
    fromEnum _ = error "fromEnum: out of Int range"

    enumFrom x        = enumDeltaNatural      x (NatS# 1##)
    enumFromThen x y
      | x <= y        = enumDeltaNatural      x (y-x)
      | otherwise     = enumNegDeltaToNatural x (x-y) (NatS# 0##)

    enumFromTo x lim  = enumDeltaToNatural    x (NatS# 1##) lim
    enumFromThenTo x y lim
      | x <= y        = enumDeltaToNatural    x (y-x) lim
      | otherwise     = enumNegDeltaToNatural x (x-y) lim

----------------------------------------------------------------------------
-- Helpers for 'Enum Natural'; TODO: optimise & make fusion work

enumDeltaNatural :: Natural -> Natural -> [Natural]
enumDeltaNatural !x d = x : enumDeltaNatural (x+d) d

enumDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumDeltaToNatural x0 delta lim = go x0
  where
    go x | x > lim   = []
         | otherwise = x : go (x+delta)

enumNegDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumNegDeltaToNatural x0 ndelta lim = go x0
  where
    go x | x < lim     = []
         | x >= ndelta = x : go (x-ndelta)
         | otherwise   = [x]

----------------------------------------------------------------------------

instance Integral Natural where
    toInteger (NatS# w)  = wordToInteger w
    toInteger (NatJ# bn) = Jp# bn

    divMod = quotRem
    div    = quot
    mod    = rem

    quotRem _ (NatS# 0##) = throw DivideByZero
    quotRem n (NatS# 1##) = (n,NatS# 0##)
    quotRem n@(NatS# _) (NatJ# _) = (NatS# 0##, n)
    quotRem (NatS# n) (NatS# d) = case quotRem (W# n) (W# d) of
        (q,r) -> (wordToNatural q, wordToNatural r)
    quotRem (NatJ# n) (NatS# d) = case quotRemBigNatWord n d of
        (# q,r #) -> (bigNatToNatural q, NatS# r)
    quotRem (NatJ# n) (NatJ# d) = case quotRemBigNat n d of
        (# q,r #) -> (bigNatToNatural q, bigNatToNatural r)

    quot _       (NatS# 0##) = throw DivideByZero
    quot n       (NatS# 1##) = n
    quot (NatS# _) (NatJ# _) = NatS# 0##
    quot (NatS# n) (NatS# d) = wordToNatural (quot (W# n) (W# d))
    quot (NatJ# n) (NatS# d) = bigNatToNatural (quotBigNatWord n d)
    quot (NatJ# n) (NatJ# d) = bigNatToNatural (quotBigNat n d)

    rem _         (NatS# 0##) = throw DivideByZero
    rem _         (NatS# 1##) = NatS# 0##
    rem n@(NatS# _) (NatJ# _) = n
    rem   (NatS# n) (NatS# d) = wordToNatural (rem (W# n) (W# d))
    rem   (NatJ# n) (NatS# d) = NatS# (remBigNatWord n d)
    rem   (NatJ# n) (NatJ# d) = bigNatToNatural (remBigNat n d)

instance Ix Natural where
    range (m,n) = [m..n]
    inRange (m,n) i = m <= i && i <= n
    unsafeIndex (m,_) i = fromIntegral (i-m)
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = indexError b i "Natural"


instance Bits Natural where
    NatS# n .&. NatS# m = wordToNatural (W# n .&. W# m)
    NatS# n .&. NatJ# m = wordToNatural (W# n .&. W# (bigNatToWord m))
    NatJ# n .&. NatS# m = wordToNatural (W# (bigNatToWord n) .&. W# m)
    NatJ# n .&. NatJ# m = bigNatToNatural (andBigNat n m)

    NatS# n .|. NatS# m = wordToNatural (W# n .|. W# m)
    NatS# n .|. NatJ# m = NatJ# (orBigNat (wordToBigNat n) m)
    NatJ# n .|. NatS# m = NatJ# (orBigNat n (wordToBigNat m))
    NatJ# n .|. NatJ# m = NatJ# (orBigNat n m)

    NatS# n `xor` NatS# m = wordToNatural (W# n `xor` W# m)
    NatS# n `xor` NatJ# m = NatJ# (xorBigNat (wordToBigNat n) m)
    NatJ# n `xor` NatS# m = NatJ# (xorBigNat n (wordToBigNat m))
    NatJ# n `xor` NatJ# m = bigNatToNatural (xorBigNat n m)

    complement _ = error "Bits.complement: Natural complement undefined"

    bitSizeMaybe _ = Nothing
    bitSize = error "Natural: bitSize"
    isSigned _ = False

    bit i@(I# i#) | i < finiteBitSize (0::Word) = wordToNatural (bit i)
                  | otherwise                   = NatJ# (bitBigNat i#)

    testBit (NatS# w) i = testBit (W# w) i
    testBit (NatJ# bn) (I# i#) = testBitBigNat bn i#

    -- TODO: setBit, clearBit, complementBit (needs more primitives)

    shiftL n           0 = n
    shiftL (NatS# 0##) _ = NatS# 0##
    shiftL (NatS# 1##) i = bit i
    shiftL (NatS# w) (I# i#)
        = bigNatToNatural $ shiftLBigNat (wordToBigNat w) i#
    shiftL (NatJ# bn) (I# i#)
        = bigNatToNatural $ shiftLBigNat bn i#

    shiftR n          0       = n
    shiftR (NatS# w)  i       = wordToNatural $ shiftR (W# w) i
    shiftR (NatJ# bn) (I# i#) = bigNatToNatural (shiftRBigNat bn i#)

    rotateL = shiftL
    rotateR = shiftR

    popCount (NatS# w)  = popCount (W# w)
    popCount (NatJ# bn) = I# (popCountBigNat bn)

    zeroBits = NatS# 0##

----------------------------------------------------------------------------

-- | 'Natural' Addition
plusNatural :: Natural -> Natural -> Natural
plusNatural (NatS# 0##) y         = y
plusNatural x         (NatS# 0##) = x
plusNatural (NatS# x) (NatS# y)
    = case plusWord2# x y of
       (# 0##, l #) -> NatS# l
       (# h,   l #) -> NatJ# (wordToBigNat2 h l)
plusNatural (NatS# x) (NatJ# y) = NatJ# (plusBigNatWord y x)
plusNatural (NatJ# x) (NatS# y) = NatJ# (plusBigNatWord x y)
plusNatural (NatJ# x) (NatJ# y) = NatJ# (plusBigNat     x y)

-- | 'Natural' multiplication
timesNatural :: Natural -> Natural -> Natural
timesNatural _         (NatS# 0##) = NatS# 0##
timesNatural (NatS# 0##) _         = NatS# 0##
timesNatural x         (NatS# 1##) = x
timesNatural (NatS# 1##) y         = y
timesNatural (NatS# x) (NatS# y) = case timesWord2# x y of
    (# 0##, 0## #) -> NatS# 0##
    (# 0##, xy  #) -> NatS# xy
    (# h  , l   #) -> NatJ# $ wordToBigNat2 h l
timesNatural (NatS# x) (NatJ# y) = NatJ# $ timesBigNatWord y x
timesNatural (NatJ# x) (NatS# y) = NatJ# $ timesBigNatWord x y
timesNatural (NatJ# x) (NatJ# y) = NatJ# $ timesBigNat     x y

-- | 'Natural' subtraction. May @'throw' 'Underflow'@.
minusNatural :: Natural -> Natural -> Natural
minusNatural x         (NatS# 0##) = x
minusNatural (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> NatS# l
    _           -> throw Underflow
minusNatural (NatS# _) (NatJ# _) = throw Underflow
minusNatural (NatJ# x) (NatS# y)
    = bigNatToNatural $ minusBigNatWord x y
minusNatural (NatJ# x) (NatJ# y)
    = bigNatToNatural $ minusBigNat     x y

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x         (NatS# 0##) = Just x
minusNaturalMaybe (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> Just (NatS# l)
    _           -> Nothing
  where
minusNaturalMaybe (NatS# _) (NatJ# _) = Nothing
minusNaturalMaybe (NatJ# x) (NatS# y)
    = Just $ bigNatToNatural $ minusBigNatWord x y
minusNaturalMaybe (NatJ# x) (NatJ# y)
  | isTrue# (isNullBigNat# res) = Nothing
  | otherwise = Just (bigNatToNatural res)
  where
    res = minusBigNat x y

-- | Helper for 'minusNatural' and 'minusNaturalMaybe'
subWordC# :: Word# -> Word# -> (# Word#, Int# #)
subWordC# x# y# = (# d#, c# #)
  where
    d# = x# `minusWord#` y#
    c# = d# `gtWord#` x#

-- | Convert 'BigNat' to 'Natural'.
-- Throws 'Underflow' if passed a 'nullBigNat'.
bigNatToNatural :: BigNat -> Natural
bigNatToNatural bn
  | isTrue# (sizeofBigNat# bn ==# 1#) = NatS# (bigNatToWord bn)
  | isTrue# (isNullBigNat# bn)        = throw Underflow
  | otherwise                         = NatJ# bn

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w#) = wordToBigNat w#
naturalToBigNat (NatJ# bn) = bn

-- | Convert 'Int' to 'Natural'.
-- Throws 'Underflow' when passed a negative 'Int'.
intToNatural :: Int -> Natural
intToNatural i | i<0 = throw Underflow
intToNatural (I# i#) = NatS# (int2Word# i#)

naturalToWord :: Natural -> Word
naturalToWord (NatS# w#) = W# w#
naturalToWord (NatJ# bn) = W# (bigNatToWord bn)

naturalToInt :: Natural -> Int
naturalToInt (NatS# w#) = I# (word2Int# w#)
naturalToInt (NatJ# bn) = I# (bigNatToInt bn)

#else /* !HAVE_GMP_BIGNAT */
----------------------------------------------------------------------------
-- Use wrapped 'Integer' as fallback; taken from Edward Kmett's nats package

-- | Type representing arbitrary-precision non-negative integers.
--
-- Operations whose result would be negative
-- @'throw' ('Underflow' :: 'ArithException')@.
--
-- @since 4.8.0.0
newtype Natural = Natural Integer -- ^ __Invariant__: non-negative 'Integer'
                deriving (Eq,Ord,Ix)

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (Natural i) = i >= 0

instance Read Natural where
    readsPrec d = map (\(n, s) -> (Natural n, s))
                  . filter ((>= 0) . (\(x,_)->x)) . readsPrec d

instance Show Natural where
    showsPrec d (Natural i) = showsPrec d i

instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  {-# INLINE (+) #-}
  Natural n * Natural m = Natural (n * m)
  {-# INLINE (*) #-}
  Natural n - Natural m | result < 0 = throw Underflow
                        | otherwise  = Natural result
    where result = n - m
  {-# INLINE (-) #-}
  abs (Natural n) = Natural n
  {-# INLINE abs #-}
  signum (Natural n) = Natural (signum n)
  {-# INLINE signum #-}
  fromInteger n
    | n >= 0 = Natural n
    | otherwise = throw Underflow
  {-# INLINE fromInteger #-}

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x y
  | x >= y    = Just (x - y)
  | otherwise = Nothing

instance Bits Natural where
  Natural n .&. Natural m = Natural (n .&. m)
  {-# INLINE (.&.) #-}
  Natural n .|. Natural m = Natural (n .|. m)
  {-# INLINE (.|.) #-}
  xor (Natural n) (Natural m) = Natural (xor n m)
  {-# INLINE xor #-}
  complement _ = error "Bits.complement: Natural complement undefined"
  {-# INLINE complement #-}
  shift (Natural n) = Natural . shift n
  {-# INLINE shift #-}
  rotate (Natural n) = Natural . rotate n
  {-# INLINE rotate #-}
  bit = Natural . bit
  {-# INLINE bit #-}
  setBit (Natural n) = Natural . setBit n
  {-# INLINE setBit #-}
  clearBit (Natural n) = Natural . clearBit n
  {-# INLINE clearBit #-}
  complementBit (Natural n) = Natural . complementBit n
  {-# INLINE complementBit #-}
  testBit (Natural n) = testBit n
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Nothing
  {-# INLINE bitSizeMaybe #-}
  bitSize = error "Natural: bitSize"
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  shiftL (Natural n) = Natural . shiftL n
  {-# INLINE shiftL #-}
  shiftR (Natural n) = Natural . shiftR n
  {-# INLINE shiftR #-}
  rotateL (Natural n) = Natural . rotateL n
  {-# INLINE rotateL #-}
  rotateR (Natural n) = Natural . rotateR n
  {-# INLINE rotateR #-}
  popCount (Natural n) = popCount n
  {-# INLINE popCount #-}
  zeroBits = Natural 0

instance Real Natural where
  toRational (Natural a) = toRational a
  {-# INLINE toRational #-}

instance Enum Natural where
  pred (Natural 0) = error "Natural.pred: 0"
  pred (Natural n) = Natural (pred n)
  {-# INLINE pred #-}
  succ (Natural n) = Natural (succ n)
  {-# INLINE succ #-}
  fromEnum (Natural n) = fromEnum n
  {-# INLINE fromEnum #-}
  toEnum n | n < 0     = error "Natural.toEnum: negative"
           | otherwise = Natural (toEnum n)
  {-# INLINE toEnum #-}

  enumFrom     = coerce (enumFrom     :: Integer -> [Integer])
  enumFromThen x y
    | x <= y    = coerce (enumFromThen :: Integer -> Integer -> [Integer]) x y
    | otherwise = enumFromThenTo x y 0

  enumFromTo   = coerce (enumFromTo   :: Integer -> Integer -> [Integer])
  enumFromThenTo
    = coerce (enumFromThenTo :: Integer -> Integer -> Integer -> [Integer])

instance Integral Natural where
  quot (Natural a) (Natural b) = Natural (quot a b)
  {-# INLINE quot #-}
  rem (Natural a) (Natural b) = Natural (rem a b)
  {-# INLINE rem #-}
  div (Natural a) (Natural b) = Natural (div a b)
  {-# INLINE div #-}
  mod (Natural a) (Natural b) = Natural (mod a b)
  {-# INLINE mod #-}
  divMod (Natural a) (Natural b) = (Natural q, Natural r)
    where (q,r) = divMod a b
  {-# INLINE divMod #-}
  quotRem (Natural a) (Natural b) = (Natural q, Natural r)
    where (q,r) = quotRem a b
  {-# INLINE quotRem #-}
  toInteger (Natural a) = a
  {-# INLINE toInteger #-}
#endif

-- | Construct 'Natural' from 'Word' value.
--
-- @since 4.8.0.0
wordToNatural :: Word -> Natural
#if HAVE_GMP_BIGNAT
wordToNatural (W# w#) = NatS# w#
#else
wordToNatural w = Natural (fromIntegral w)
#endif

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns 'Nothing' if value doesn't fit in 'Word'.
--
-- @since 4.8.0.0
naturalToWordMaybe :: Natural -> Maybe Word
#if HAVE_GMP_BIGNAT
naturalToWordMaybe (NatS# w#) = Just (W# w#)
naturalToWordMaybe (NatJ# _)  = Nothing
#else
naturalToWordMaybe (Natural i)
  | i <= maxw  = Just (fromIntegral i)
  | otherwise  = Nothing
  where
    maxw = toInteger (maxBound :: Word)
#endif

-- This follows the same style as the other integral 'Data' instances
-- defined in "Data.Data"
naturalType :: DataType
naturalType = mkIntType "Numeric.Natural.Natural"

instance Data Natural where
  toConstr x = mkIntegralConstr naturalType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Natural"
  dataTypeOf _ = naturalType

-- | \"@'powModNatural' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- @since 4.8.0.0
powModNatural :: Natural -> Natural -> Natural -> Natural
#if HAVE_GMP_BIGNAT
powModNatural _           _           (NatS# 0##) = throw DivideByZero
powModNatural _           _           (NatS# 1##) = NatS# 0##
powModNatural _           (NatS# 0##) _           = NatS# 1##
powModNatural (NatS# 0##) _           _           = NatS# 0##
powModNatural (NatS# 1##) _           _           = NatS# 1##
powModNatural (NatS# b)   (NatS# e)   (NatS# m)   = NatS# (powModWord b e m)
powModNatural b           e           (NatS# m)
  = NatS# (powModBigNatWord (naturalToBigNat b) (naturalToBigNat e) m)
powModNatural b           e           (NatJ# m)
  = bigNatToNatural (powModBigNat (naturalToBigNat b) (naturalToBigNat e) m)
#else
-- Portable reference fallback implementation
powModNatural _ _ 0 = throw DivideByZero
powModNatural _ _ 1 = 0
powModNatural _ 0 _ = 1
powModNatural 0 _ _ = 0
powModNatural 1 _ _ = 1
powModNatural b0 e0 m = go b0 e0 1
  where
    go !b e !r
      | odd e     = go b' e' (r*b `mod` m)
      | e == 0    = r
      | otherwise = go b' e' r
      where
        b' = b*b `mod` m
        e' = e   `unsafeShiftR` 1 -- slightly faster than "e `div` 2"
#endif
