-- |
-- Module      : Basement.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- Types to represent ℤ/nℤ.
--
-- ℤ/nℤ is a finite field and is defined as the set of natural number:
-- {0, 1, ..., n − 1}.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Basement.Bounded
    ( Zn64
    , unZn64
    , Zn
    , unZn
    , zn64
    , zn
    , zn64Nat
    , znNat
    ) where

import           GHC.TypeLits
import           Data.Word
import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Numerical.Number
import           Data.Proxy
import           Basement.Nat
import qualified Prelude

-- | A type level bounded natural backed by a Word64
newtype Zn64 (n :: Nat) = Zn64 { unZn64 :: Word64 }
    deriving (Show,Eq,Ord)

instance (KnownNat n, NatWithinBound Word64 n) => Prelude.Num (Zn64 n) where
    fromInteger = zn64 . Prelude.fromInteger
    (+) = add64
    (-) = sub64
    (*) = mul64
    abs a = a
    negate _ = error "cannot negate Zn64: use Foundation Numerical hierarchy for this function to not be exposed to Zn64"
    signum (Zn64 a) = Zn64 (Prelude.signum a)

type instance NatNumMaxBound (Zn64 n) = n

instance (KnownNat n, NatWithinBound Word64 n) => Integral (Zn64 n) where
    fromInteger = zn64 . Prelude.fromInteger
instance (KnownNat n, NatWithinBound Word64 n) => IsIntegral (Zn64 n) where
    toInteger (Zn64 n) = toInteger n
instance (KnownNat n, NatWithinBound Word64 n) => IsNatural (Zn64 (n :: Nat)) where
    toNatural (Zn64 n) = toNatural n

-- | Create an element of ℤ/nℤ from a Word64
--
-- If the value is greater than n, then the value is normalized by using the
-- integer modulus n
zn64 :: forall n . (KnownNat n, NatWithinBound Word64 n) => Word64 -> Zn64 n
zn64 v = Zn64 (v `Prelude.mod` natValWord64 (Proxy :: Proxy n))

-- | Create an element of ℤ/nℤ from a type level Nat
zn64Nat :: forall m n . (KnownNat m, KnownNat n, NatWithinBound Word64 m, NatWithinBound Word64 n, CmpNat m n ~ 'LT)
        => Proxy m
        -> Zn64 n
zn64Nat p = Zn64 (natValWord64 p)

-- | Add 2 Zn64
add64 :: forall n . (KnownNat n, NatWithinBound Word64 n) => Zn64 n -> Zn64 n -> Zn64 n
add64 (Zn64 a) (Zn64 b) = Zn64 ((a Prelude.+ b) `Prelude.mod` natValWord64 (Proxy :: Proxy n))

-- | subtract 2 Zn64
sub64 :: forall n . (KnownNat n, NatWithinBound Word64 n) => Zn64 n -> Zn64 n -> Zn64 n
sub64 (Zn64 a) (Zn64 b) = Zn64 ((a Prelude.- b) `Prelude.mod` natValWord64 (Proxy :: Proxy n))

-- | Multiply 2 Zn64
mul64 :: forall n . (KnownNat n, NatWithinBound Word64 n) => Zn64 n -> Zn64 n -> Zn64 n
mul64 (Zn64 a) (Zn64 b) = Zn64 ((a Prelude.* b) `Prelude.mod` natValWord64 (Proxy :: Proxy n))

-- | A type level bounded natural
newtype Zn (n :: Nat) = Zn { unZn :: Natural }
    deriving (Show,Eq,Ord)

instance KnownNat n => Prelude.Num (Zn n) where
    fromInteger = zn . Prelude.fromInteger
    (+) = add
    (-) = sub
    (*) = mul
    abs a = a
    negate _ = error "cannot negate Zn: use Foundation Numerical hierarchy for this function to not be exposed to Zn"
    signum = Zn . Prelude.signum . unZn

type instance NatNumMaxBound (Zn n) = n

instance KnownNat n => Integral (Zn n) where
    fromInteger = zn . Prelude.fromInteger
instance KnownNat n => IsIntegral (Zn n) where
    toInteger (Zn n) = toInteger n
instance KnownNat n => IsNatural (Zn n) where
    toNatural i = unZn i

-- | Create an element of ℤ/nℤ from a Natural.
--
-- If the value is greater than n, then the value is normalized by using the
-- integer modulus n
zn :: forall n . KnownNat n => Natural -> Zn n
zn v = Zn (v `Prelude.mod` natValNatural (Proxy :: Proxy n))

-- | Create an element of ℤ/nℤ from a type level Nat
znNat :: forall m n . (KnownNat m, KnownNat n, CmpNat m n ~ 'LT) => Proxy m -> Zn n
znNat m = Zn (natValNatural m)

-- | Add 2 Zn
add :: forall n . KnownNat n => Zn n -> Zn n -> Zn n
add (Zn a) (Zn b) = Zn ((a Prelude.+ b) `Prelude.mod` natValNatural (Proxy :: Proxy n))

-- | subtract 2 Zn
sub :: forall n . KnownNat n => Zn n -> Zn n -> Zn n
sub (Zn a) (Zn b) = Zn ((a Prelude.- b) `Prelude.mod` natValNatural (Proxy :: Proxy n))

-- | Multiply 2 Zn
mul :: forall n . KnownNat n => Zn n -> Zn n -> Zn n
mul (Zn a) (Zn b) = Zn ((a Prelude.* b) `Prelude.mod` natValNatural (Proxy :: Proxy n))

