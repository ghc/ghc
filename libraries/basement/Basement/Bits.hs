-- |
-- Module      : Basement.Bits
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NegativeLiterals #-}

#include "MachDeps.h"

module Basement.Bits
    ( BitOps(..)
    , FiniteBitsOps(..)
    , Bits
    , toBits
    , allOne
    ) where

import Basement.Compat.Base
import Basement.Compat.Natural
import Basement.Numerical.Additive
import Basement.Numerical.Subtractive
import Basement.Numerical.Multiplicative
import Basement.Types.OffsetSize
import Basement.Types.Word128 (Word128)
import qualified Basement.Types.Word128 as Word128
import Basement.Types.Word256 (Word256)
import qualified Basement.Types.Word256 as Word256
import Basement.IntegralConv (wordToInt)
import Basement.Nat

import qualified Prelude
import qualified Data.Bits as OldBits
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Base hiding ((.))
import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Int

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

-- | operation over finite bits
class FiniteBitsOps bits where
    -- | get the number of bits in the given object
    --
    numberOfBits :: bits -> CountOf Bool

    -- | rotate the given bit set.
    rotateL :: bits -> CountOf Bool -> bits
    -- | rotate the given bit set.
    rotateR :: bits -> CountOf Bool -> bits

    -- | count of number of bit set to 1 in the given bit set.
    popCount :: bits -> CountOf Bool

    -- | reverse all bits in the argument
    bitFlip   :: bits -> bits

    -- | count of the number of leading zeros
    countLeadingZeros :: bits -> CountOf Bool
    default countLeadingZeros :: BitOps bits => bits -> CountOf Bool
    countLeadingZeros n = loop stop azero
      where
        stop = numberOfBits n
        loop idx count
            | idx == azero = count
            | isBitSet n (sizeAsOffset idx) = count
            | otherwise = loop (fromMaybe azero (idx - 1)) (count + 1)

    -- | count of the number of trailing zeros
    countTrailingZeros :: bits -> CountOf Bool
    default countTrailingZeros :: BitOps bits => bits -> CountOf Bool
    countTrailingZeros n = loop azero
      where
        stop = numberOfBits n
        loop count
            | count == stop = count
            | isBitSet n (sizeAsOffset count) = count
            | otherwise = loop (count + 1)

-- | operation over bits
class BitOps bits where
    (.&.)     :: bits -> bits -> bits
    (.|.)     :: bits -> bits -> bits
    (.^.)     :: bits -> bits -> bits
    (.<<.)    :: bits -> CountOf Bool -> bits
    (.>>.)    :: bits -> CountOf Bool -> bits
    -- | construct a bit set with the bit at the given index set.
    bit       :: Offset Bool -> bits
    default bit :: Integral bits => Offset Bool -> bits
    bit n = 1 .<<. (offsetAsSize n)

    -- | test the bit at the given index is set
    isBitSet  :: bits -> Offset Bool -> Bool
    default isBitSet :: (Integral bits, Eq bits) => bits -> Offset Bool -> Bool
    isBitSet x n = x .&. (bit n) /= 0

    -- | set the bit at the given index
    setBit    :: bits -> Offset Bool -> bits
    default setBit :: Integral bits => bits -> Offset Bool -> bits
    setBit x n = x .|. (bit n)

    -- | clear the bit at the given index
    clearBit  :: bits -> Offset Bool -> bits
    default clearBit :: FiniteBitsOps bits => bits -> Offset Bool -> bits
    clearBit x n = x .&. (bitFlip (bit n))

infixl 8 .<<., .>>., `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.

-- | Bool set of 'n' bits.
--
newtype Bits (n :: Nat) = Bits { bitsToNatural :: Natural }
  deriving (Show, Eq, Ord, Typeable)

-- | convenient Type Constraint Alias fot 'Bits' functions
type SizeValid n = (KnownNat n, 1 <= n)

-- convert an 'Int' into a 'Natural'.
-- This functions is not meant to be exported
lift :: Int -> Natural
lift = Prelude.fromIntegral
{-# INLINABLE lift #-}

-- | convert the given 'Natural' into a 'Bits' of size 'n'
--
-- if bits that are not within the boundaries of the 'Bits n' will be truncated.
toBits :: SizeValid n => Natural -> Bits n
toBits nat = Bits nat .&. allOne

-- | construct a 'Bits' with all bits set.
--
-- this function is equivalet to 'maxBound'
allOne :: forall n . SizeValid n => Bits n
allOne = Bits (2 Prelude.^ n Prelude.- midentity)
  where
    n = natVal (Proxy @n)

instance SizeValid n => Enum (Bits n) where
    toEnum i | i < 0 && lift i > bitsToNatural maxi = error "Bits n not within bound"
             | otherwise                            = Bits (lift i)
      where maxi = allOne :: Bits n
    fromEnum (Bits n) = fromEnum n
instance SizeValid n => Bounded (Bits n) where
    minBound = azero
    maxBound = allOne
instance SizeValid n => Additive (Bits n) where
    azero = Bits 0
    (+) (Bits a) (Bits b) = toBits (a + b)
    scale n (Bits a) = toBits (scale n a)
instance SizeValid n => Subtractive (Bits n) where
    type Difference (Bits n) = Bits n
    (-) (Bits a) (Bits b) = maybe azero toBits (a - b)
instance SizeValid n => Multiplicative (Bits n) where
    midentity = Bits 1
    (*) (Bits a) (Bits b) = Bits (a Prelude.* b)
instance SizeValid n => IDivisible (Bits n) where
    div (Bits a) (Bits b) = Bits (a `Prelude.div` b)
    mod (Bits a) (Bits b) = Bits (a `Prelude.mod` b)
    divMod (Bits a) (Bits b) = let (q, r) = Prelude.divMod a b in (Bits q, Bits r)

instance SizeValid n => BitOps (Bits n) where
    (.&.)    (Bits a) (Bits b)    = Bits (a OldBits..&. b)
    (.|.)    (Bits a) (Bits b)    = Bits (a OldBits..|. b)
    (.^.)    (Bits a) (Bits b)    = Bits (a `OldBits.xor` b)
    (.<<.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftL` w)
    (.>>.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftR` w)
    bit               (Offset w)  = Bits (OldBits.bit w)
    isBitSet (Bits a) (Offset w)  = OldBits.testBit a w
    setBit   (Bits a) (Offset w)  = Bits (OldBits.setBit a w)
    clearBit (Bits a) (Offset w)  = Bits (OldBits.clearBit a w)
instance (SizeValid n, NatWithinBound (CountOf Bool) n) => FiniteBitsOps (Bits n) where
    bitFlip (Bits a) = Bits (OldBits.complement a)
    numberOfBits _ = natValCountOf (Proxy @n)
    rotateL a i = (a .<<. i) .|. (a .>>. d)
      where
        n = natValCountOf (Proxy :: Proxy n)
        d = fromMaybe (fromMaybe (error "impossible") (i - n)) (n - i)
    rotateR a i = (a .>>. i) .|. (a .<<. d)
      where
        n = natValCountOf (Proxy :: Proxy n)
        d = fromMaybe (fromMaybe (error "impossible") (i - n)) (n - i)
    popCount (Bits n) = CountOf (OldBits.popCount n)

-- Bool ------------------------------------------------------------------------

instance FiniteBitsOps Bool where
    numberOfBits _ = 1
    rotateL x _ = x
    rotateR x _ = x
    popCount True = 1
    popCount False = 0
    bitFlip  = not
    countLeadingZeros True  = 0
    countLeadingZeros False = 1
    countTrailingZeros True  = 0
    countTrailingZeros False = 1
instance BitOps Bool where
    (.&.) = (&&)
    (.|.) = (||)
    (.^.) = (/=)
    x .<<. 0 = x
    _ .<<. _ = False
    x .>>. 0 = x
    _ .>>. _ = False
    bit 0 = True
    bit _ = False
    isBitSet x 0 = x
    isBitSet _ _ = False
    setBit _ 0 = True
    setBit _ _ = False
    clearBit _ 0 = False
    clearBit x _ = x

-- Word8 ----------------------------------------------------------------------

instance FiniteBitsOps Word8 where
    numberOfBits _ = 8
    rotateL (W8# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W8# x#
        | otherwise  = W8# (narrow8Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                          (x# `uncheckedShiftRL#` (8# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    rotateR (W8# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W8# x#
        | otherwise  = W8# (narrow8Word# ((x# `uncheckedShiftRL#` i'#) `or#`
                                          (x# `uncheckedShiftL#` (8# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    bitFlip (W8# x#) = W8# (x# `xor#` mb#)
        where !(W8# mb#) = maxBound
    popCount (W8# x#) = CountOf $ wordToInt (W# (popCnt8# x#))
    countLeadingZeros (W8# w#) = CountOf $ wordToInt (W# (clz8# w#))
    countTrailingZeros (W8# w#) = CountOf $ wordToInt (W# (ctz8# w#))
instance BitOps Word8 where
    (W8# x#) .&. (W8# y#)   = W8# (x# `and#` y#)
    (W8# x#) .|. (W8# y#)   = W8# (x# `or#`  y#)
    (W8# x#) .^. (W8# y#)   = W8# (x# `xor#` y#)
    (W8# x#) .<<. (CountOf (I# i#)) = W8# (narrow8Word# (x# `shiftL#` i#))
    (W8# x#) .>>. (CountOf (I# i#)) = W8# (narrow8Word# (x# `shiftRL#` i#))

-- Word16 ---------------------------------------------------------------------

instance FiniteBitsOps Word16 where
    numberOfBits _ = 16
    rotateL (W16# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W16# x#
        | otherwise  = W16# (narrow16Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (16# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    rotateR (W16# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W16# x#
        | otherwise  = W16# (narrow16Word# ((x# `uncheckedShiftRL#` i'#) `or#`
                                            (x# `uncheckedShiftL#` (16# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitFlip (W16# x#) = W16# (x# `xor#` mb#)
        where !(W16# mb#) = maxBound
    popCount (W16# x#) = CountOf $ wordToInt (W# (popCnt16# x#))
    countLeadingZeros (W16# w#) = CountOf $ wordToInt (W# (clz16# w#))
    countTrailingZeros (W16# w#) = CountOf $ wordToInt (W# (ctz16# w#))
instance BitOps Word16 where
    (W16# x#) .&. (W16# y#)   = W16# (x# `and#` y#)
    (W16# x#) .|. (W16# y#)   = W16# (x# `or#`  y#)
    (W16# x#) .^. (W16# y#)   = W16# (x# `xor#` y#)
    (W16# x#) .<<. (CountOf (I# i#)) = W16# (narrow16Word# (x# `shiftL#` i#))
    (W16# x#) .>>. (CountOf (I# i#)) = W16# (narrow16Word# (x# `shiftRL#` i#))

-- Word32 ---------------------------------------------------------------------

instance FiniteBitsOps Word32 where
    numberOfBits _ = 32
    rotateL (W32# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W32# x#
        | otherwise  = W32# (narrow32Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (32# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    rotateR (W32# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W32# x#
        | otherwise  = W32# (narrow32Word# ((x# `uncheckedShiftRL#` i'#) `or#`
                                            (x# `uncheckedShiftL#` (32# -# i'#))))
      where
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    bitFlip (W32# x#) = W32# (x# `xor#` mb#)
        where !(W32# mb#) = maxBound
    popCount (W32# x#) = CountOf $ wordToInt (W# (popCnt32# x#))
    countLeadingZeros (W32# w#) = CountOf $ wordToInt (W# (clz32# w#))
    countTrailingZeros (W32# w#) = CountOf $ wordToInt (W# (ctz32# w#))
instance BitOps Word32 where
    (W32# x#) .&. (W32# y#)   = W32# (x# `and#` y#)
    (W32# x#) .|. (W32# y#)   = W32# (x# `or#`  y#)
    (W32# x#) .^. (W32# y#)   = W32# (x# `xor#` y#)
    (W32# x#) .<<. (CountOf (I# i#)) = W32# (narrow32Word# (x# `shiftL#` i#))
    (W32# x#) .>>. (CountOf (I# i#)) = W32# (narrow32Word# (x# `shiftRL#` i#))

-- Word ---------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Word where
    numberOfBits _ = 64
    rotateL (W# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftL#` i'#) `or#`
                           (x# `uncheckedShiftRL#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    rotateR (W# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftRL#` i'#) `or#`
                           (x# `uncheckedShiftL#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitFlip (W# x#) = W# (x# `xor#` mb#)
        where !(W# mb#) = maxBound
    popCount (W# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W# w#) = CountOf $ wordToInt (W# (ctz64# w#))
#else
instance FiniteBitsOps Word where
    numberOfBits _ = 32
    rotateL (W# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftL#` i'#) `or#`
                           (x# `uncheckedShiftRL#` (32# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    rotateR (W# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftRL#` i'#) `or#`
                           (x# `uncheckedShiftL#` (32# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    bitFlip (W# x#) = W# (x# `xor#` mb#)
        where !(W# mb#) = maxBound
    popCount (W# x#) = CountOf $ wordToInt (W# (popCnt32# x#))
    countLeadingZeros (W# w#) = CountOf $ wordToInt (W# (clz32# w#))
    countTrailingZeros (W# w#) = CountOf $ wordToInt (W# (ctz32# w#))
#endif

instance BitOps Word where
    (W# x#) .&. (W# y#)   = W# (x# `and#` y#)
    (W# x#) .|. (W# y#)   = W# (x# `or#`  y#)
    (W# x#) .^. (W# y#)   = W# (x# `xor#` y#)
    (W# x#) .<<. (CountOf (I# i#)) = W# ((x# `shiftL#` i#))
    (W# x#) .>>. (CountOf (I# i#)) = W# ((x# `shiftRL#` i#))

-- Word64 ---------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Word64 where
    numberOfBits _ = 64
    rotateL (W64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise  = W64# ((x# `uncheckedShiftL#` i'#) `or#`
                             (x# `uncheckedShiftRL#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    rotateR (W64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise  = W64# ((x# `uncheckedShiftRL#` i'#) `or#`
                             (x# `uncheckedShiftL#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitFlip (W64# x#) = W64# (x# `xor#` mb#)
        where !(W64# mb#) = maxBound
    popCount (W64# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W64# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W64# w#) = CountOf $ wordToInt (W# (ctz64# w#))
instance BitOps Word64 where
    (W64# x#) .&. (W64# y#)   = W64# (x# `and#` y#)
    (W64# x#) .|. (W64# y#)   = W64# (x# `or#`  y#)
    (W64# x#) .^. (W64# y#)   = W64# (x# `xor#` y#)
    (W64# x#) .<<. (CountOf (I# i#)) = W64# (x# `shiftL#` i#)
    (W64# x#) .>>. (CountOf (I# i#)) = W64# (x# `shiftRL#` i#)
#else
instance FiniteBitsOps Word64 where
    numberOfBits _ = 64
    rotateL (W64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise  = W64# ((x# `uncheckedShiftL64#` i'#) `or64#`
                             (x# `uncheckedShiftRL64#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    rotateR (W64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = W64# x#
        | otherwise  = W64# ((x# `uncheckedShiftRL64#` i'#) `or64#`
                             (x# `uncheckedShiftL64#` (64# -# i'#)))
      where
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitFlip (W64# x#) = W64# (not64# x#)
    popCount (W64# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W64# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W64# w#) = CountOf $ wordToInt (W# (ctz64# w#))
instance BitOps Word64 where
    (W64# x#) .&. (W64# y#)   = W64# (x# `and64#` y#)
    (W64# x#) .|. (W64# y#)   = W64# (x# `or64#`  y#)
    (W64# x#) .^. (W64# y#)   = W64# (x# `xor64#` y#)
    (W64# x#) .<<. (CountOf (I# i#)) = W64# (x# `shiftL64#` i#)
    (W64# x#) .>>. (CountOf (I# i#)) = W64# (x# `shiftRL64#` i#)

shiftL64#, shiftRL64# :: Word64# -> Int# -> Word64#
a `shiftL64#` b  | isTrue# (b >=# 64#) = wordToWord64# 0##
                 | otherwise           = a `uncheckedShiftL64#` b
a `shiftRL64#` b | isTrue# (b >=# 64#) = wordToWord64# 0##
                 | otherwise           = a `uncheckedShiftRL64#` b
#endif

-- Word128 --------------------------------------------------------------------

instance FiniteBitsOps Word128 where
    numberOfBits _ = 128
    rotateL w (CountOf n) = Word128.rotateL w n
    rotateR w (CountOf n) = Word128.rotateR w n
    bitFlip = Word128.complement
    popCount = CountOf . Word128.popCount
instance BitOps Word128 where
    (.&.) = Word128.bitwiseAnd
    (.|.) = Word128.bitwiseOr
    (.^.) = Word128.bitwiseXor
    (.<<.) w (CountOf n) = Word128.shiftL w n
    (.>>.) w (CountOf n) = Word128.shiftR w n

-- Word256 --------------------------------------------------------------------

instance FiniteBitsOps Word256 where
    numberOfBits _ = 256
    rotateL w (CountOf n) = Word256.rotateL w n
    rotateR w (CountOf n) = Word256.rotateR w n
    bitFlip = Word256.complement
    popCount = CountOf . Word256.popCount
instance BitOps Word256 where
    (.&.) = Word256.bitwiseAnd
    (.|.) = Word256.bitwiseOr
    (.^.) = Word256.bitwiseXor
    (.<<.) w (CountOf n) = Word256.shiftL w n
    (.>>.) w (CountOf n) = Word256.shiftR w n

-- Int8 -----------------------------------------------------------------------

instance FiniteBitsOps Int8 where
    numberOfBits _ = 8
    rotateL (I8# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I8# x#
        | otherwise  = I8# (narrow8Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                                    (x'# `uncheckedShiftRL#` (8# -# i'#)))))
      where
        !x'# = narrow8Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    rotateR (I8# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I8# x#
        | otherwise  = I8# (narrow8Int# (word2Int# ((x'# `uncheckedShiftRL#` i'#) `or#`
                                                    (x'# `uncheckedShiftL#` (8# -# i'#)))))
      where
        !x'# = narrow8Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    bitFlip (I8# x#) = I8# (word2Int# (not# (int2Word# x#)))
    popCount (I8# x#) = CountOf $ wordToInt (W# (popCnt8# (int2Word# x#)))
    countLeadingZeros (I8# w#) = CountOf $ wordToInt (W# (clz8# (int2Word# w#)))
    countTrailingZeros (I8# w#) = CountOf $ wordToInt (W# (ctz8# (int2Word# w#)))
instance BitOps Int8 where
    (I8# x#) .&. (I8# y#)   = I8# (x# `andI#` y#)
    (I8# x#) .|. (I8# y#)   = I8# (x# `orI#`  y#)
    (I8# x#) .^. (I8# y#)   = I8# (x# `xorI#` y#)
    (I8# x#) .<<. (CountOf (I# i#)) = I8# (narrow8Int# (x# `iShiftL#`  i#))
    (I8# x#) .>>. (CountOf (I# i#)) = I8# (narrow8Int# (x# `iShiftRL#` i#))

-- Int16 ----------------------------------------------------------------------

instance FiniteBitsOps Int16 where
    numberOfBits _ = 16
    rotateL (I16# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I16# x#
        | otherwise  = I16# (narrow16Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                                      (x'# `uncheckedShiftRL#` (16# -# i'#)))))
      where
        !x'# = narrow16Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    rotateR (I16# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I16# x#
        | otherwise  = I16# (narrow16Int# (word2Int# ((x'# `uncheckedShiftRL#` i'#) `or#`
                                                      (x'# `uncheckedShiftL#` (16# -# i'#)))))
      where
        !x'# = narrow16Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitFlip (I16# x#) = I16# (word2Int# (not# (int2Word# x#)))
    popCount (I16# x#) = CountOf $ wordToInt (W# (popCnt16# (int2Word# x#)))
    countLeadingZeros (I16# w#) = CountOf $ wordToInt (W# (clz16# (int2Word# w#)))
    countTrailingZeros (I16# w#) = CountOf $ wordToInt (W# (ctz16# (int2Word# w#)))
instance BitOps Int16 where
    (I16# x#) .&. (I16# y#)   = I16# (x# `andI#` y#)
    (I16# x#) .|. (I16# y#)   = I16# (x# `orI#`  y#)
    (I16# x#) .^. (I16# y#)   = I16# (x# `xorI#` y#)
    (I16# x#) .<<. (CountOf (I# i#)) = I16# (narrow16Int# (x# `iShiftL#`  i#))
    (I16# x#) .>>. (CountOf (I# i#)) = I16# (narrow16Int# (x# `iShiftRL#` i#))

-- Int32 ----------------------------------------------------------------------

instance FiniteBitsOps Int32 where
    numberOfBits _ = 32
    rotateL (I32# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I32# x#
        | otherwise  = I32# (narrow32Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                                      (x'# `uncheckedShiftRL#` (32# -# i'#)))))
      where
        !x'# = narrow32Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    rotateR (I32# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I32# x#
        | otherwise  = I32# (narrow32Int# (word2Int# ((x'# `uncheckedShiftRL#` i'#) `or#`
                                                      (x'# `uncheckedShiftL#` (32# -# i'#)))))
      where
        !x'# = narrow32Word# (int2Word# x#)
        !i'# = word2Int# (int2Word# i# `and#` 31##)
    bitFlip (I32# x#) = I32# (word2Int# (not# (int2Word# x#)))
    popCount (I32# x#) = CountOf $ wordToInt (W# (popCnt32# (int2Word# x#)))
    countLeadingZeros (I32# w#) = CountOf $ wordToInt (W# (clz32# (int2Word# w#)))
    countTrailingZeros (I32# w#) = CountOf $ wordToInt (W# (ctz32# (int2Word# w#)))
instance BitOps Int32 where
    (I32# x#) .&. (I32# y#)   = I32# (x# `andI#` y#)
    (I32# x#) .|. (I32# y#)   = I32# (x# `orI#`  y#)
    (I32# x#) .^. (I32# y#)   = I32# (x# `xorI#` y#)
    (I32# x#) .<<. (CountOf (I# i#)) = I32# (narrow32Int# (x# `iShiftL#`  i#))
    (I32# x#) .>>. (CountOf (I# i#)) = I32# (narrow32Int# (x# `iShiftRL#` i#))

-- Int64 ----------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Int64 where
    numberOfBits _ = 64
    rotateL (I64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I64# x#
        | otherwise  = I64# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                        (x'# `uncheckedShiftRL#` (64# -# i'#))))
      where
        !x'# = int2Word# x#
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    rotateR (I64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I64# x#
        | otherwise  = I64# (word2Int# ((x'# `uncheckedShiftRL#` i'#) `or#`
                                        (x'# `uncheckedShiftL#` (64# -# i'#))))
      where
        !x'# = int2Word# x#
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitFlip (I64# x#) = I64# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    popCount (I64# x#) = CountOf $ wordToInt (W# (popCnt64# (int2Word# x#)))
    countLeadingZeros (I64# w#) = CountOf $ wordToInt (W# (clz64# (int2Word# w#)))
    countTrailingZeros (I64# w#) = CountOf $ wordToInt (W# (ctz64# (int2Word# w#)))
instance BitOps Int64 where
    (I64# x#) .&. (I64# y#)   = I64# (x# `andI#` y#)
    (I64# x#) .|. (I64# y#)   = I64# (x# `orI#`  y#)
    (I64# x#) .^. (I64# y#)   = I64# (x# `xorI#` y#)
    (I64# x#) .<<. (CountOf (I# w#)) = I64# (x# `iShiftL#`  w#)
    (I64# x#) .>>. (CountOf (I# w#)) = I64# (x# `iShiftRL#` w#)
#else
instance FiniteBitsOps Int64 where
    numberOfBits _ = 64
    rotateL (I64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I64# x#
        | otherwise  = I64# (word64ToInt64# ((x'# `uncheckedShiftL64#` i'#) `or64#`
                                             (x'# `uncheckedShiftRL64#` (64# -# i'#))))
      where
        !x'# = int64ToWord64# x#
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    rotateR (I64# x#) (CountOf (I# i#))
        | isTrue# (i'# ==# 0#) = I64# x#
        | otherwise  = I64# (word64ToInt64# ((x'# `uncheckedShiftRL64#` i'#) `or64#`
                                             (x'# `uncheckedShiftL64#` (64# -# i'#))))
      where
        !x'# = int64ToWord64# x#
        !i'# = word2Int# (int2Word# i# `and#` 63##)
    bitFlip (I64# x#) = I64# (word64ToInt64# (not64# (int64ToWord64# x#)))
    popCount (I64# x#) = CountOf $ wordToInt (W# (popCnt64# (int64ToWord64# x#)))
    countLeadingZeros (I64# w#) = CountOf $ wordToInt (W# (clz64# (int64ToWord64# w#)))
    countTrailingZeros (I64# w#) = CountOf $ wordToInt (W# (ctz64# (int64ToWord64# w#)))
instance BitOps Int64 where
    (I64# x#) .&. (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `and64#` int64ToWord64# y#))
    (I64# x#) .|. (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `or64#`  int64ToWord64# y#))
    (I64# x#) .^. (I64# y#)  = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` int64ToWord64# y#))
    (I64# x#) .<<. (CountOf (I# w#)) = I64# (x# `iShiftL64#`  w#)
    (I64# x#) .>>. (CountOf (I# w#)) = I64# (x# `iShiftRA64#` w#)


iShiftL64#, iShiftRA64# :: Int64# -> Int# -> Int64#
a `iShiftL64#` b  | isTrue# (b >=# 64#) = intToInt64# 0#
                  | otherwise           = a `uncheckedIShiftL64#` b
a `iShiftRA64#` b | isTrue# (b >=# 64#) && isTrue# (a `ltInt64#` (intToInt64# 0#))
                                        = intToInt64# (-1#)
                  | isTrue# (b >=# 64#) = intToInt64# 0#
                  | otherwise = a `uncheckedIShiftRA64#` b

#endif
