{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module T14366 where
import Data.Kind
import Data.Type.Equality

type family Cast (a :: Type) (b :: Type) (e :: a :~: b) (x :: a) :: b where
  Cast _ _ Refl x = x

type family F (a :: Type) :: Type where
  F (a :: _) = a
