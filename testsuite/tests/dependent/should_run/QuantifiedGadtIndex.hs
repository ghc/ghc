{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications #-}
module QuantifiedGadtIndex where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type Slot :: forall (a :: Bool). Proxy a -> Type
data Slot p = Slot

type Pick :: forall k. (forall (a :: k). Proxy a -> Type) -> Type
data Pick f where
  PickBool :: Pick @Bool Slot

type Carrier :: forall k. (forall (a :: k). Proxy a -> Type) -> Type
type role Carrier nominal
data Carrier f = Carrier Int

transport :: forall k (f :: forall (a :: k). Proxy a -> Type).
  Pick @k f -> Carrier @k f -> Carrier @Bool Slot
transport PickBool x = x
