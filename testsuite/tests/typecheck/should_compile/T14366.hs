{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TopLevelKindSignatures #-}

module T14366 where

import Data.Kind
import Data.Type.Equality

type Cast :: forall (a :: Type) (b :: Type) -> a :~: b -> a -> b
type family Cast a b e x where
  Cast _ _ Refl x = x

type F :: Type -> Type
type family F a where
  F (a :: _) = a
