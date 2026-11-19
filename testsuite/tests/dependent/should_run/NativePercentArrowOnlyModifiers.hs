{-# LANGUAGE DataKinds, Modifiers, NoLinearTypes, NoExperimentalDependentTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module NativePercentArrowOnlyModifiers where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U, M)

identity :: a %U -> a
identity x = x

explicit :: (Int %M -> Bool) :~: ((->) @M Int Bool)
explicit = Refl
