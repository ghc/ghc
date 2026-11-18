{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenSource (x, y, z, implicitlyBound, explicitlyBound, first, second, repeated) where

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
