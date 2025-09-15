{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module T14366 where
import Data.Kind
import Data.Type.Equality

type Cast :: forall a b -> (a :~: b) -> (a -> b)
type family Cast a b e x where
  Cast _ _ Refl x = x

type family F (a :: Type) :: Type where
  F (a :: _) = a
