{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, TypeOperators #-}
module MappingProof (mapNothing, mapJust, example) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import MappingWorker (mapLike)
import ConcreteGuardedListSource (Negated)

-- | The empty case is independent of the callback.
mapNothing :: forall a b (f :: a %U -> b). Proxy f
           -> mapLike f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
mapNothing _ = Refl

-- | The equation holds for any callback and any present value.
mapJust :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
        -> mapLike f ('Just x) :~: 'Just (f x)
mapJust _ _ = Refl

-- | Show the two proofs beside their runtime results.
example :: String
example = show ( mapNothing (Proxy :: Proxy Negated)
               , mapJust (Proxy :: Proxy Negated) (Proxy :: Proxy 'True)
               , mapLike (error "unused callback") Nothing :: Maybe Bool
               , mapLike not (Just True) )
