{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module PackageEquality (sameRetained) where

import Data.Type.Equality ((:~:)(Refl))
import ForgetWitness (Flag, Retained)

-- | Declare the imported sum again with a different binder name.
type AlsoRetained = some (index :: Bool) | Flag index

-- | Reduce only when both arguments match the same pattern variable.
type family Same a b where
  Same a a = Bool

-- | Use the repeated family argument with two independently written sums.
sameRetained :: Same Retained AlsoRetained :~: Bool
sameRetained = Refl
