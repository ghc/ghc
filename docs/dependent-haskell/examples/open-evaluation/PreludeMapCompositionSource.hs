{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures, TypeFamilies #-}
module PreludeMapCompositionSource (ComposedMap) where

import GHC.Exts (U)
import PreludeMapLengthSource (Mapped)

-- | Apply the outer callback to the result of the inner callback.
type Compose :: forall a b c. (b %U -> c) -> (a %U -> b) -> a %U -> c
type family Compose f g x where
    Compose f g x = f (g x)

-- | Map once with the two callbacks composed.
type ComposedMap :: forall a b c. (b %U -> c) -> (a %U -> b) -> [a] -> [c]
type ComposedMap f g xs = Mapped (Compose f g) xs
