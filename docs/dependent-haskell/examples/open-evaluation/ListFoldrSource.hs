{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module ListFoldrSource (Folded) where

import GHC.Exts (U)
import qualified GHC.Internal.List as List

-- | Use the installed list fold with an arbitrary combiner and seed.
type Folded :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type Folded k z xs = List.foldr k z xs
