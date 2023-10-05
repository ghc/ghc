{-# LANGUAGE ImpredicativeTypes, TypeFamilies, UndecidableInstances #-}
module CoerceToVDQ where

import Data.Kind
import GHC.Exts (UnliftedType)

import Data.Coerce

type S0 = forall (k :: Type) -> k -> k

type family S where
  -- S just smuggles S0 past the validity checks
  -- that normally prevent VDQ in types of terms
  S = ArgKind (S0Consumer S0Inhabitant)

type ArgKind :: Type -> Type
type family ArgKind a where
  ArgKind (_ (arg :: k)) = k

type S0Inhabitant :: S0
data family S0Inhabitant a b

type S0Consumer :: S0 -> Type
newtype S0Consumer f = Con (f Type Bool)

test :: S
test = coerce @(forall t. t -> t) id
