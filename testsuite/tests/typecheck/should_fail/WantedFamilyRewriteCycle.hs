{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies #-}
module WantedFamilyRewriteCycle where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

type family Width (b :: Bool) :: Bool
type family Payload (b :: Bool) :: Type

-- The first Refl asks for q ~ p. It can rewrite the arguments of Width
-- in the second Wanted but cannot rewrite the Given itself.
badDirect :: forall p q. Width p ~ Width q
          => Proxy p -> Proxy q -> (q :~: p, Width q :~: Width p)
badDirect _ _ = (Refl, Refl)

badNested :: forall p q. Payload p ~ Maybe (Payload q)
          => Proxy p -> Proxy q -> (q :~: p, Payload p :~: Maybe (Payload q))
badNested _ _ = (Refl, Refl)
