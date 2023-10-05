{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import Data.Kind
import Data.Type.Equality

data Nat = Z | S Nat

data SNat :: Nat -> Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

type family S' (n :: Nat) :: Nat where
  S' n = S n

data R :: Nat -> Nat -> Nat -> Type where
  MkR :: !(R m n o) -> R (S m) n (S o)

type family NatPlus (m :: Nat) (n :: Nat) :: Nat where
  NatPlus Z     n = n
  NatPlus (S m) n = S' (NatPlus m n)

f :: forall (m :: Nat) (n :: Nat) (o :: Nat).
     SNat m -> SNat n -> SNat o
  -> R m n o -> NatPlus m n :~: o
f (SS sm) sn (SS so) (MkR r)
  | Refl <- f sm sn so r
  = Refl
