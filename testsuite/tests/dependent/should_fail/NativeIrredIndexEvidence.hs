{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeIrredIndexEvidence where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

type family Flip (b :: Bool) :: Bool where
    Flip 'True = 'False
    Flip 'False = 'True

data Token (b :: Bool) = Token Bool
type View :: forall (a :: Bool) -> forall (b :: Bool) -> Token b -> Type
data View a b v where
    View :: forall a (v :: Token (Flip a)). Proxy v -> View a (Flip a) v

-- The two inputs have the same index. Their identities can differ.
wrong :: foreach (a :: Bool) -> foreach (b :: Bool) -> foreach (v :: Token b)
      -> foreach (other :: Token b) -> Flip a :~: b -> View a b v
wrong a b v other Refl = View @a (Proxy @other)
