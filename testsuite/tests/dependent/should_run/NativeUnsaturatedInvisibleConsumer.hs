{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeUnsaturatedInvisibleConsumer where
import Data.Type.Equality
import InvisibleFamily (Observed, ObservedVisible, Test)

carry :: Test -> Test
carry x = x

observed :: Observed :~: 'True
observed = Refl

observedVisible :: ObservedVisible :~: Int
observedVisible = Refl
