{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-}
module NativePartialSynonym where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)

type Column err a = Either err (Maybe a)
type Drop (unused :: Type) a = a

type Slot :: (Type %U -> Type) -> Type
data Slot f = Slot

type Saved err = Slot (Column err)

type Apply :: (Type %U -> Type) -> Type -> Type
type family Apply f a where Apply f a = f a

atChar :: Apply (Column Bool) Char :~: Either Bool (Maybe Char)
atChar = Refl

congruent :: a :~: b -> Saved a :~: Saved b
congruent Refl = Refl

-- Check a forwarding chain and a higher-order parameter at the same kind.
type Again e a = Column e a
type Twice e a = Again e a
type Through (f :: Type %U -> Type) a = f a

forwarded :: Slot (Twice Bool) :~: Saved Bool
forwarded = Refl

forwardedVariable :: Slot (Through f) :~: Slot f
forwardedVariable = Refl
