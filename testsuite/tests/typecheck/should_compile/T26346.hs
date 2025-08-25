{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T26346 (warble) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))

type Nat :: Type
data Nat = Z | S Nat

type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

type NatPlus :: Nat -> Nat -> Nat
type family NatPlus a b where
  NatPlus Z     b = b
  NatPlus (S a) b = S (NatPlus a b)

sNatPlus ::
  forall (a :: Nat) (b :: Nat).
  SNat a ->
  SNat b ->
  SNat (NatPlus a b)
sNatPlus SZ     b = b
sNatPlus (SS a) b = SS (sNatPlus a b)

data Bin
  = Zero
  | Even Bin
  | Odd Bin

type SBin :: Bin -> Type
data SBin b where
  SZero :: SBin Zero
  SEven :: SBin n -> SBin (Even n)
  SOdd  :: SBin n -> SBin (Odd n)

type Incr :: Bin -> Bin
type family Incr b where
  Incr Zero     = Odd Zero      -- 0 + 1 = (2*0) + 1
  Incr (Even n) = Odd n         -- 2n + 1
  Incr (Odd n)  = Even (Incr n) -- (2n + 1) + 1 = 2*(n + 1)

type BinToNat :: Bin -> Nat
type family BinToNat b where
  BinToNat Zero     = Z
  BinToNat (Even n) = NatPlus (BinToNat n) (BinToNat n)
  BinToNat (Odd n)  = S (NatPlus (BinToNat n) (BinToNat n))

sBinToNat ::
  forall (b :: Bin).
  SBin b ->
  SNat (BinToNat b)
sBinToNat SZero     = SZ
sBinToNat (SEven n) = sNatPlus (sBinToNat n) (sBinToNat n)
sBinToNat (SOdd n)  = SS (sNatPlus (sBinToNat n) (sBinToNat n))

warble ::
  forall (b :: Bin).
  SBin b ->
  BinToNat (Incr b) :~: S (BinToNat b)
warble SZero = Refl
warble (SEven {}) = Refl
warble (SOdd  sb) | Refl <- warble sb
                  , Refl <- plusComm sbn (SS sbn)
                  = Refl
  where
    sbn = sBinToNat sb

    plus0R ::
      forall (n :: Nat).
      SNat n ->
      NatPlus n Z :~: n
    plus0R SZ = Refl
    plus0R (SS sn)
      | Refl <- plus0R sn
      = Refl

    plusSnR ::
      forall (n :: Nat) (m :: Nat).
      SNat n ->
      SNat m ->
      NatPlus n (S m) :~: S (NatPlus n m)
    plusSnR SZ _ = Refl
    plusSnR (SS sn) sm
      | Refl <- plusSnR sn sm
      = Refl

    plusComm ::
      forall (n :: Nat) (m :: Nat).
      SNat n ->
      SNat m ->
      NatPlus n m :~: NatPlus m n
    plusComm SZ sm
      | Refl <- plus0R sm
      = Refl
    plusComm (SS sn) sm
      | Refl <- plusComm sn sm
      , Refl <- plusSnR sm sn
      = Refl
