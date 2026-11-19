{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, ScopedTypeVariables #-}
module OpenSource (x, y, z, implicitlyBound, explicitlyBound, first, second, repeated, routedFirst, routedSecond, tupleFirst, tupleSecond, workerFirst, ignoresRigidPair) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified OpenProducer
-- | Use the upstream unqualified source syntax under an explicit forall.
x :: forall a. Num a => id a
x = 42

-- | Use the same function with its written qualifier.
y :: forall a. Num a => Prelude.id a
y = 42

-- | Reduce a different checked term definition.
z :: forall a. Num a => OpenProducer.select a
z = 42

-- | Keep the existing implicitly bound type constructor spelling.
implicitlyBound :: Proxy (id a)
implicitlyBound = Proxy

-- | A type binder takes precedence over the imported term.
explicitlyBound :: forall id a. Proxy (id a)
explicitlyBound = Proxy

-- | Apply Prelude const to distinct type skolems.
first :: forall a b. a -> b -> const a b
first x' _ = x'

-- | Select the second of two different type skolems.
second :: forall a b. a -> b -> OpenProducer.second a b
second _ y' = y'

-- | Supply the same skolem in both operands.
repeated :: forall a. a -> OpenProducer.second a a
repeated x' = x'

-- | Inspect a known character before returning the first rigid operand.
routedFirst :: forall a b. a -> b -> OpenProducer.routeLeft a b
routedFirst x' _ = x'

-- | Use the default branch to return the second rigid operand.
routedSecond :: forall a b. a -> b -> OpenProducer.routeRight a b
routedSecond _ y' = y'

-- | Select the first field of a promoted pair of distinct rigid operands.
tupleFirst :: forall a b. a -> b -> Prelude.fst '(a, b)
tupleFirst x' _ = x'

-- | Select the second field through the ordinary imported Prelude definition.
tupleSecond :: forall a b. a -> b -> Prelude.snd '(a, b)
tupleSecond _ y' = y'

-- | Use an independent worker to check interface dependency on the projection.
workerFirst :: forall a b. a -> b -> OpenProducer.pairFirst '(a, b)
workerFirst x' _ = x'

-- | A function that ignores a pair can return another rigid operand.
ignoresRigidPair :: forall (p :: (Type, Type)) a. Proxy p -> a -> OpenProducer.ignorePair p a
ignoresRigidPair _ x' = x'
