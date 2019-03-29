{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE GADTs, PolyKinds, ExplicitForAll #-}

module TLKS_005 where

import Data.Kind (Type, Constraint)

type TypeRep :: forall k. k -> Type
data TypeRep a where
  TyInt   :: TypeRep Int
  TyMaybe :: TypeRep Maybe
  TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)
