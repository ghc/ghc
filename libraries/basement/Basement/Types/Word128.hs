{-# LANGUAGE CPP                #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE UnboxedTuples      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Basement.Types.Word128
    ( Word128(..)
    , (+)
    , (-)
    , (*)
    , quot
    , rem
    , bitwiseAnd
    , bitwiseOr
    , bitwiseXor
    , complement
    , shiftL
    , shiftR
    , rotateL
    , rotateR
    , popCount
    , fromNatural
    ) where

import           GHC.Prim
import           GHC.Word
import           GHC.Types
import qualified Prelude (fromInteger, show, Num(..), quot, rem, mod)
import           Data.Bits hiding (complement, popCount, bit, testBit
                                  , rotateL, rotateR, shiftL, shiftR)
import qualified Data.Bits as Bits
import           Data.Function (on)
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable

import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Compat.Primitive (bool#)
import           Basement.Numerical.Conversion
import           Basement.Numerical.Number

#include "MachDeps.h"

-- | 128 bits Word
data Word128 = Word128 {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
    deriving (Eq, Typeable)

instance Show Word128 where
    show w = Prelude.show (toNatural w)
instance Enum Word128 where
    toEnum i = Word128 0 $ int64ToWord64 (intToInt64 i)
    fromEnum (Word128 _ a0) = wordToInt (word64ToWord a0)
    succ (Word128 a1 a0)
        | a0 == maxBound = Word128 (succ a1) 0
        | otherwise      = Word128 a1        (succ a0)
    pred (Word128 a1 a0)
        | a0 == minBound = Word128 (pred a1) maxBound
        | otherwise      = Word128 a1        (pred a0)
instance Bounded Word128 where
    minBound = Word128 minBound minBound
    maxBound = Word128 maxBound maxBound
instance Ord Word128 where
    compare (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> compare a0 b0
            r  -> r
    (<) (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> a0 < b0
            r  -> r == LT
    (<=) (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> a0 <= b0
            r  -> r == LT
instance Storable Word128 where
    sizeOf _ = 16
    alignment _ = 16
    peek p = Word128 <$> peek (castPtr p            )
                     <*> peek (castPtr p `plusPtr` 8)
    poke p (Word128 a1 a0) = do
        poke (castPtr p            ) a1
        poke (castPtr p `plusPtr` 8) a0

instance Integral Word128 where
    fromInteger = literal
instance HasNegation Word128 where
    negate = complement

instance IsIntegral Word128 where
    toInteger (Word128 a1 a0) =
        (toInteger a1 `unsafeShiftL` 64) .|.
        toInteger a0
instance IsNatural Word128 where
    toNatural (Word128 a1 a0) =
        (toNatural a1 `unsafeShiftL` 64) .|.
        toNatural a0

instance Prelude.Num Word128 where
    abs w = w
    signum w@(Word128 a1 a0)
        | a1 == 0 && a0 == 0 = w
        | otherwise          = Word128 0 1
    fromInteger = literal
    (+) = (+)
    (-) = (-)
    (*) = (*)

instance Bits.Bits Word128 where
    (.&.) = bitwiseAnd
    (.|.) = bitwiseOr
    xor   = bitwiseXor
    complement = complement
    shiftL = shiftL
    shiftR = shiftR
    rotateL = rotateL
    rotateR = rotateR
    bitSize _ = 128
    bitSizeMaybe _ = Just 128
    isSigned _ = False
    testBit = testBit
    bit = bit
    popCount = popCount

-- | Add 2 Word128
(+) :: Word128 -> Word128 -> Word128
#if WORD_SIZE_IN_BITS < 64
(+) = applyBiWordOnNatural (Prelude.+)
#else
(+) (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) = Word128 (W64# s1) (W64# s0)
  where
    !(# carry, s0 #) = plusWord2# a0 b0
    s1               = plusWord# (plusWord# a1 b1) carry
#endif

-- temporary available until native operation available
applyBiWordOnNatural :: (Natural -> Natural -> Natural)
                     -> Word128
                     -> Word128
                     -> Word128
applyBiWordOnNatural f a b = fromNatural $ f (toNatural a) (toNatural b)

-- | Subtract 2 Word128
(-) :: Word128 -> Word128 -> Word128
(-) a b
    | a >= b    = applyBiWordOnNatural (Prelude.-) a b
    | otherwise = complement (applyBiWordOnNatural (Prelude.-) b a) + 1

-- | Multiplication
(*) :: Word128 -> Word128 -> Word128
(*) = applyBiWordOnNatural (Prelude.*)

-- | Division
quot :: Word128 -> Word128 -> Word128
quot = applyBiWordOnNatural Prelude.quot

-- | Modulo
rem :: Word128 -> Word128 -> Word128
rem = applyBiWordOnNatural Prelude.rem

-- | Bitwise and
bitwiseAnd :: Word128 -> Word128 -> Word128
bitwiseAnd (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 .&. b1) (a0 .&. b0)

-- | Bitwise or
bitwiseOr :: Word128 -> Word128 -> Word128
bitwiseOr (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 .|. b1) (a0 .|. b0)

-- | Bitwise xor
bitwiseXor :: Word128 -> Word128 -> Word128
bitwiseXor (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 `Bits.xor` b1) (a0 `Bits.xor` b0)

-- | Bitwise complement
complement :: Word128 -> Word128
complement (Word128 a1 a0) = Word128 (Bits.complement a1) (Bits.complement a0)

-- | Population count
popCount :: Word128 -> Int
popCount (Word128 a1 a0) = Bits.popCount a1 Prelude.+ Bits.popCount a0

-- | Bitwise Shift Left
shiftL :: Word128 -> Int -> Word128
shiftL w@(Word128 a1 a0) n
    | n < 0 || n > 127 = Word128 0 0
    | n == 64          = Word128 a0 0
    | n == 0           = w
    | n >  64          = Word128 (a0 `Bits.unsafeShiftL` (n Prelude.- 64)) 0
    | otherwise        = Word128 ((a1 `Bits.unsafeShiftL` n) .|. (a0 `Bits.unsafeShiftR` (64 Prelude.- n)))
                                 (a0 `Bits.unsafeShiftL` n)

-- | Bitwise Shift Right
shiftR :: Word128 -> Int -> Word128
shiftR w@(Word128 a1 a0) n
    | n < 0 || n > 127 = Word128 0 0
    | n == 64          = Word128 0 a1
    | n == 0           = w
    | n >  64          = Word128 0 (a1 `Bits.unsafeShiftR` (n Prelude.- 64))
    | otherwise        = Word128 (a1 `Bits.unsafeShiftR` n)
                                 ((a1 `Bits.unsafeShiftL` (inv64 n)) .|. (a0 `Bits.unsafeShiftR` n))

-- | Bitwise rotate Left
rotateL :: Word128 -> Int -> Word128
rotateL (Word128 a1 a0) n'
    | n == 0    = Word128 a1 a0
    | n == 64   = Word128 a0 a1
    | n < 64    = Word128 (comb64 a1 n a0 (inv64 n)) (comb64 a0 n a1 (inv64 n))
    | otherwise = let nx = n Prelude.- 64 in Word128 (comb64 a0 nx a1 (inv64 nx)) (comb64 a1 n' a0 (inv64 nx))
  where
    n :: Int
    n | n' >= 0   = n' `Prelude.mod` 128
      | otherwise = 128 Prelude.- (n' `Prelude.mod` 128)

-- | Bitwise rotate Left
rotateR :: Word128 -> Int -> Word128
rotateR w n = rotateL w (128 Prelude.- n)

inv64 :: Int -> Int
inv64 i = 64 Prelude.- i

comb64 :: Word64 -> Int -> Word64 -> Int -> Word64
comb64 x i y j =
    (x `Bits.unsafeShiftL` i) .|. (y `Bits.unsafeShiftR` j)

-- | Test bit
testBit :: Word128 -> Int -> Bool
testBit (Word128 a1 a0) n
    | n < 0 || n > 127 = False
    | n > 63           = Bits.testBit a1 (n Prelude.- 64)
    | otherwise        = Bits.testBit a0 n

-- | bit
bit :: Int -> Word128
bit n
    | n < 0 || n > 127 = Word128 0 0
    | n > 63           = Word128 (Bits.bit (n Prelude.- 64)) 0
    | otherwise        = Word128 0 (Bits.bit n)

literal :: Integer -> Word128
literal i = Word128
    (Prelude.fromInteger (i `Bits.unsafeShiftR` 64))
    (Prelude.fromInteger i)

fromNatural :: Natural -> Word128
fromNatural n = Word128
    (Prelude.fromInteger (naturalToInteger n `Bits.unsafeShiftR` 64))
    (Prelude.fromInteger $ naturalToInteger n)
