{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module OpenBuildSource (Make, exact, uncurried, nested, rigid, pair, swapped, Swap) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import qualified OpenBuildWorker as B

-- | The result type computed from two unknown inputs.
type Make :: Type -> Type -> B.Shell Type Type
type Make a b = B.build a b

-- | Check the constructor and both fields.
exact :: forall a b. Proxy a -> Proxy b -> Make a b :~: 'B.Shell a b
exact _ _ = Refl

-- | Check the constructor callback with both input fields unknown.
uncurried :: forall a b. Proxy a -> Proxy b -> B.buildWithUncurry a b :~: 'B.Shell a b
uncurried _ _ = Refl

-- | Check both layers of the constructed result.
nested :: forall a b. Proxy a -> Proxy b -> B.wrap a b :~: 'B.Nested ('B.Shell a b)
nested _ _ = Refl

-- | An unknown input can pass through the ordinary identity.
rigid :: forall (p :: B.Shell Type Type). Proxy p -> B.echo p :~: p
rigid _ = Refl

-- | Check construction of a boxed pair.
pair :: forall a b. Proxy a -> Proxy b -> B.buildPair a b :~: '(a, b)
pair _ _ = Refl

-- | Check both field positions after swapping a known pair.
swapped :: forall a b. Proxy a -> Proxy b -> B.swapPair '(a, b) :~: '(b, a)
swapped _ _ = Refl

-- | Export the call even when the pair constructor is unknown here.
type Swap :: (k, l) -> (l, k)
type Swap p = B.swapPair p
