{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module PreludeMapLengthSource (Count, Mapped) where

import GHC.Exts (U)
import qualified Prelude as P
import qualified PreludeMapLengthSize as S

-- | Refer to the checked equations of the runtime size function.
type Count :: forall a. [a] -> S.Size
type Count xs = S.size xs

-- | Use the installed map with any callback at the element arrow.
type Mapped :: forall a b. (a %U -> b) -> [a] -> [b]
type Mapped f xs = P.map f xs
