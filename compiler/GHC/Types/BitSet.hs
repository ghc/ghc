{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

#include "MachDeps.h"
#define WORD_SIZE_IN_BITS 64

module GHC.Types.BitSet
    ( BitSet
    , empty
    , fromList
    , toList
    , member
    , filter
    , union
    , difference
    , intersection
    ) where

import Data.Bits
import Data.Foldable (foldl')
import Data.Kind
import Data.Proxy
import Data.Coerce
import GHC.TypeLits
import Data.Array.ST
import GHC.Prelude hiding (filter)
import GHC.Utils.Outputable hiding (empty)

#if WORD_SIZE_IN_BITS == 64
type WordSize = 64
wordSize = 64
#elif WORD_SIZE_IN_BITS == 32
type WordSize = 32
wordSize = 32
#else
#error invalid word size
#endif

class (Bits (Words n), KnownNat n) => NWords (n :: Nat) where
    data family Words (n :: Nat) :: Type

instance {-# OVERLAPPABLE #-} NWords 0 where
    data Words 0 = W0

deriving instance Eq (Words 0)
deriving instance Show (Words 0)

instance Bits (Words 0) where
  zeroBits = W0
  bit _ = W0
  testBit _ _ = False
  clearBit _ _ = W0
  setBit _ _ = W0
  _ .&. _ = W0
  _ .|. _ = W0
  _ `xor` _ = W0
  complement _ = W0
  shift _ _ = W0
  rotate _ _ = W0
  bitSize _ = 0
  bitSizeMaybe _ = Just 0
  isSigned _ = False
  popCount _ = 0

instance {-# OVERLAPPING #-} NWords 1 where
    newtype Words 1 = W1 Word

deriving instance Eq (Words 1)
deriving instance Show (Words 1)

instance Bits (Words 1) where
  zeroBits = W1 0
  bit n = W1 (bit n)
  testBit (W1 a0) = testBit a0
  clearBit (W1 a0) n = W1 $ clearBit a0 n
  setBit (W1 a0) n = W1 $ setBit a0 n
  W1 a0 .&. W1 b0 = W1 (a0 .&. b0)
  W1 a0 .|. W1 b0 = W1 (a0 .|. b0)
  W1 a0 `xor` W1 b0 = W1 (a0 `xor` b0)
  complement (W1 a0) = W1 (complement a0)
  shift = undefined
  rotate = undefined
  bitSize = undefined
  bitSizeMaybe = undefined
  isSigned = undefined
  popCount = undefined

instance {-# OVERLAPPING #-} NWords 2 where
    data Words 2 = W2 !Word !Word

deriving instance Eq (Words 2)
deriving instance Show (Words 2)

instance Bits (Words 2) where
  zeroBits = W2 0 0
  bit n
    | n < wordSize = W2 (bit n) 0
    | otherwise = W2 0 (bit (n-wordSize))
  testBit (W2 a0 a1) n
    | n < wordSize = testBit a0 n
    | otherwise    = testBit a1 (n-64)
  W2 a0 a1 .&. W2 b0 b1 = W2 (a0 .&. b0) (a1 .&. b1)
  W2 a0 a1 .|. W2 b0 b1 = W2 (a0 .|. b0) (a1 .|. b1)
  W2 a0 a1 `xor` W2 b0 b1 = W2 (a0 `xor` b0) (a1 `xor` b1)
  complement (W2 a0 a1) = W2 (complement a0) (complement a1)
  shift = undefined
  rotate = undefined
  bitSize = undefined
  bitSizeMaybe = undefined
  isSigned = undefined
  popCount = undefined

--instance {-# OVERLAPPABLE #-} NWords n where
--    data NWords n = GenWords !ByteArray

type NumWords (n :: Nat) = (n + WordSize - 1) `Div` WordSize

type HasBitSet (n :: Nat) = NWords (NumWords n)

-- | @BitSet n@ is a set of integers in the range @[0,n)@.
newtype BitSet (n :: Nat) = BitSet (Words (NumWords n))

instance (HasBitSet n, KnownNat n) => Show (BitSet n) where
    show is = "fromList " ++ show (toList is)

instance (HasBitSet n, KnownNat n) => Outputable (BitSet n) where
    ppr = text . show

-- | Fold over all integers from @[0,n)@.
foldN :: forall n a. KnownNat n
      => Proxy (n :: Nat) -> (Int -> a -> a) -> a -> a
foldN Proxy f x0 = foldr (.) id (map f [0 .. n-1]) x0
  where n = fromInteger $ natVal (Proxy @n)

empty :: HasBitSet n => BitSet n
empty = BitSet zeroBits

member :: HasBitSet n => Int -> BitSet n -> Bool
member n (BitSet ws) = testBit ws n

filter :: forall n. (KnownNat n, HasBitSet n)
       => (Int -> Bool) -> BitSet n -> BitSet n
filter f = foldN (Proxy @n) g
  where
    g :: Int -> BitSet n -> BitSet n
    g i ws
      | member i ws
      , not (f i)
      = case ws of BitSet ws -> BitSet $ clearBit ws i

      | otherwise
      = ws

toList :: forall n. (KnownNat n, HasBitSet n) => BitSet n -> [Int]
toList ws = foldN (Proxy @n) f []
  where
    f :: Int -> [Int] -> [Int]
    f i rest | member i ws = i : rest
    f _ rest = rest

fromList :: forall n. (KnownNat n, HasBitSet n) => [Int] -> BitSet n
fromList xs = BitSet $ foldl' (.|.) zeroBits (map bit xs)

union :: (KnownNat n, HasBitSet n) => BitSet n -> BitSet n -> BitSet n
union (BitSet a) (BitSet b) = BitSet (a .|. b)

difference :: (KnownNat n, HasBitSet n) => BitSet n -> BitSet n -> BitSet n
difference (BitSet a) (BitSet b) = BitSet (a .&. complement b)

intersection :: (KnownNat n, HasBitSet n) => BitSet n -> BitSet n -> BitSet n
intersection (BitSet a) (BitSet b) = BitSet (a .&. b)

