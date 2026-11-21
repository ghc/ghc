{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeDependentResultBinders where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))

type family Flip (b :: Bool) :: Bool where
    Flip 'True = 'False
    Flip 'False = 'True

data Token (b :: Bool) = Token
type Box :: forall (b :: Bool) -> Token b -> Type
data Box b v = Box
type Payload :: forall (b :: Bool) -> forall (v :: Token b) -> Box b v -> Type
data Payload b v w = Payload

type View :: forall (a :: Bool) -> forall (b :: Bool) -> forall (v :: Token b) -> Box b v -> Type
data View a b v w where
    View :: forall a (v :: Token (Flip a)) (w :: Box (Flip a) v).
            Payload (Flip a) v w -> View a (Flip a) v w

-- The kind of the second inferred operand mentions the first.
right :: foreach (a :: Bool) -> foreach (b :: Bool) -> foreach (v :: Token b)
      -> foreach (w :: Box b v) -> Flip a :~: b -> View a b v w
right a b v w Refl = View @a (Payload @(Flip a) @v @w)
