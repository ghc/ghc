{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, RankNTypes, TypeApplications, TypeFamilies #-}
module NativeUnsaturatedInvisibleInverse where
import Data.Type.Equality
import GHC.Exts (U)

data Box (f :: forall k. @U k)

bad :: forall (f :: forall k. @U k) (g :: forall k. @U k).
       (f @Bool ~ g @Bool) => Box f :~: Box g
bad = Refl
