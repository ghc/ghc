{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures, TypeFamilies #-}
module ListFoldrSource (Folded, HeadStep) where

import GHC.Exts (U)
import qualified GHC.Internal.List as List

-- | Use the installed list fold with an arbitrary combiner and seed.
type Folded :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type Folded k z xs = List.foldr k z xs

-- | The step used in the finite example takes the head and ignores the rest.
type family First (x :: Bool) (rest :: Bool) :: Bool where
    First x rest = x

-- | Export the partially applied step to the ordinary proof module.
type HeadStep = First
