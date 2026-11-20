{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeUnsaturatedInvisibleConsumer where
import Data.Type.Equality
import InvisibleFamily (Observed, ObservedVisible, ReadFlag, ReadType, Test)

carry :: Test -> Test
carry x = x

observed :: Observed :~: 'True
observed = Refl

observedVisible :: ObservedVisible :~: Int
observedVisible = Refl

readAtTwoKinds :: (ReadFlag :~: 'True, ReadType :~: Int)
readAtTwoKinds = (Refl, Refl)
