{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeUnsaturatedInverse where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))

inverse :: forall (f :: FUN @U 'Many Bool Bool) (a :: Bool) (b :: Bool).
           Proxy f -> f a :~: f b -> a :~: b
inverse _ Refl = Refl
