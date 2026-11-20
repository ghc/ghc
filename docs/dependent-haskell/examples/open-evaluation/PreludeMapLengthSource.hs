{-# LANGUAGE DataKinds, ExperimentalDependentTypes, LinearTypes, StandaloneKindSignatures #-}
module PreludeMapLengthSource (Mapped) where

import GHC.Exts (Matchability (Unmatchable))
import qualified Prelude as P

-- | Use the installed map with any callback at the element arrow.
type Mapped :: forall a b. (a %Unmatchable -> b) -> [a] -> [b]
type Mapped f xs = P.map f xs
