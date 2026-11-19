{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeFamilies #-}
module NativeClosedMatchabilityClient where

import Data.Kind (Type)
import NativeClosedMatchabilityProvider (Forward, K)

type family Interpret (b :: Bool) :: K b
type instance Interpret 'True = Maybe

identity :: K 'True
identity x = x

data Bridge (k :: Type) (f :: k) (g :: k)
type Carries = Bridge (Forward 'True) Maybe Maybe

type Same a = a
type ForwardAgain b = Same (K b)
type Shared (b :: Bool) (f :: K b) (g :: ForwardAgain b) = Bridge (K b) f g

data Forms (b :: Bool) where
  Forms :: forall b (f :: K b). Bridge (K b) f f -> Forms b

type PairOf b c = (K b, K c)
type family First (t :: Type) :: Type where
  First (x, y) = x
type family Second (t :: Type) :: Type where
  Second (x, y) = y

type Projected (b :: Bool) (f :: First (PairOf b 'False)) (g :: K b)
  = Bridge (K b) f g

projectedRuntime :: Second (PairOf 'True 'False)
projectedRuntime x = x
