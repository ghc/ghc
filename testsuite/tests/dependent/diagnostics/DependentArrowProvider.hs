{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeApplications, TypeFamilies #-}
module DependentArrowProvider (D, Abstract, AbstractAny) where

import Data.Kind (Type)
import GHC.Exts (FUN, U, Multiplicity(Many))

-- | Let the first argument declare the kind of the second.
data D :: forall (k :: Type) -> k -> Type
-- | Provide a type whose kind has an unmatchable arrow.
type family Abstract :: FUN @U 'Many Type Type
-- | Provide a type at any requested kind.
type family AbstractAny :: k
