{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module PreludeMapLengthSource (Mapped) where

import GHC.Exts (U)
import qualified Prelude as P

-- | Use the installed map with any callback at the element arrow.
type Mapped :: forall a b. (a %U -> b) -> [a] -> [b]
type Mapped f xs = P.map f xs
