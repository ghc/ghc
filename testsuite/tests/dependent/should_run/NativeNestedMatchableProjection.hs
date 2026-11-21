{-# LANGUAGE DataKinds, GADTs, Modifiers, ScopedTypeVariables, TypeOperators #-}
module NativeNestedMatchableProjection where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)

type Twice f x = f (f x)
type Four f x = Twice f (Twice f x)
type Eight f x = Four f (Four f x)
type Sixteen f x = Eight f (Eight f x)
type ThirtyTwo f x = Sixteen f (Sixteen f x)

-- | Project through each matchable application of the same head.
cancel :: forall (f :: Type %M -> Type) a b.
          Proxy f -> ThirtyTwo f a :~: ThirtyTwo f b -> a :~: b
cancel _ Refl = Refl
