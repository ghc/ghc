{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeApplications, TypeFamilies #-}
module DependentArrowProvider (D, Abstract, AbstractAny, Callback) where

import Data.Kind (Type)
import GHC.Exts (FUN, M, U, Multiplicity(Many))

-- | Let the first argument declare the kind of the second.
data D :: forall (k :: Type) -> k -> Type
-- | Provide a type whose kind has an unmatchable arrow.
type family Abstract :: FUN @U 'Many Type Type
-- | Provide a type at any requested kind.
type family AbstractAny :: k

-- | Choose the arrow for an imported type function.
type family Arrow (b :: Bool) where
  Arrow False = FUN @M 'Many Type Type
  Arrow True  = FUN @U 'Many Type Type

-- | Hide the result arrow behind one family equation.
type family Callback (b :: Bool) :: Arrow b
