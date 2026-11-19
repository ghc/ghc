{-# LANGUAGE DataKinds, Modifiers, NoLinearTypes, NoExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, TypeApplications, TypeOperators #-}
module NativePercentArrowOnlyModifiers where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U, M)

identity :: a %U -> a
identity x = x

explicit :: (Int %M -> Bool) :~: ((->) @M Int Bool)
explicit = Refl

-- The singletons encoding uses the synonym as the kind of a data type.
data TyFun a b
type a ~> b = TyFun a b %M -> Type
data Symbol :: Type ~> Type

type Function a b = a -> b
applyIdentity :: Function Int Int
applyIdentity x = x
