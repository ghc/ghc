{-# LANGUAGE DataKinds, TypeApplications, TypeFamilies #-}
module NativeClosedMatchabilityProvider (K, Forward, Nullary, FixedNullary) where

import Data.Kind (Type)
import GHC.Exts (FUN, Multiplicity(Many), U)

type family K (b :: Bool) :: Type where
  K 'True = Type -> Type
  K 'False = Bool -> Bool

type Forward b = K b

type family Nullary where
  Nullary = Type -> Type

type family FixedNullary where
  FixedNullary = FUN @U 'Many Type Type
