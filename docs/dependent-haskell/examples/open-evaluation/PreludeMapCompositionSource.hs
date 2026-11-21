{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module PreludeMapCompositionSource (ComposedMap) where

import GHC.Exts (U)
import qualified Prelude as P
import PreludeMapLengthSource (Mapped)

-- | Map once with the installed composition operator.
type ComposedMap :: forall a b c. (b %U -> c) -> (a %U -> b) -> [a] -> [c]
type ComposedMap f g xs = Mapped (f P.. g) xs
