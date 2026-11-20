{-# LANGUAGE NoExperimentalDependentTypes, PolyKinds, TypeFamilies, TypeOperators #-}

module NestedFunctionClient (binaryChoice, conjunction) where

import Data.Kind (Type)
import NestedFunctionKind

-- | Use a binary constructor at two matchable arrows.
type family Choose (f :: Demote (Type ~> (Type ~> Type))) :: Type
type instance Choose Either = Either Bool Int

-- | Return an inhabitant selected by the interpreted constructor.
binaryChoice :: Choose Either
binaryChoice = Right 7

-- | Use the same kind interpreter to form a curried runtime function.
conjunction :: Demote (Bool ~> (Bool ~> Bool))
conjunction = (&&)
