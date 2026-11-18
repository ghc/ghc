{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenSource (x, y, z, implicitlyBound, explicitlyBound) where

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
