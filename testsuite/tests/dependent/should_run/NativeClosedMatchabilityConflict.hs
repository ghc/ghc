{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, PolyKinds, TypeApplications #-}
module NativeClosedMatchabilityConflict where
import NativeClosedMatchabilityProvider (K)
import Data.Kind (Type)
import GHC.Exts (FUN, M, U, Multiplicity(Many))

data Bridge (k :: Type) (f :: k) (g :: k)
type Wrong (f :: FUN @M 'Many Type Type) (g :: FUN @U 'Many Type Type)
  = Bridge (K 'True) f g
