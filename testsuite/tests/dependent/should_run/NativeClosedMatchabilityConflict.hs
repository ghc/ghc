{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeClosedMatchabilityConflict where
import Data.Kind (Type)

#if defined(INVERSE)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))

bad :: forall (f :: FUN @U 'Many Type Type) a b. (f a ~ f b) => a :~: b
bad = Refl
#elif defined(FIXED)
import NativeClosedMatchabilityProvider (FixedNullary)

type family Interpret (b :: Bool) :: FixedNullary
type instance Interpret 'True = Maybe
#else
import GHC.Exts (FUN, M, U, Multiplicity(Many))
import NativeClosedMatchabilityProvider (K)

data Bridge (k :: Type) (f :: k) (g :: k)
type Wrong (f :: FUN @M 'Many Type Type) (g :: FUN @U 'Many Type Type)
  = Bridge (K 'True) f g
#endif
