{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeFamilies #-}
module NativeOpenMatchabilityConflict where

import Data.Kind (Type)
import GHC.Exts (FUN, Multiplicity(Many), U)

type family Arrow (b :: Bool) :: Type
type family Apply (b :: Bool) (f :: Arrow b) :: Type

type instance Arrow 'True = FUN @U 'Many Type Type
type instance Apply 'True Maybe = Bool
