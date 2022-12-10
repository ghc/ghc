{-# LANGUAGE TypeFamilies #-}

module T15079_fail_b where

import Data.Kind (Type)
import Data.Void (Void)

infixl 4 :==
-- | Heterogeneous Leibnizian equality.
newtype (a :: j) :== (b :: k)
  = HRefl { hsubst :: forall (c :: forall (i :: Type). i -> Type). c a -> c b }

newtype Coerce a = Coerce { uncoerce :: Starify a }

type Starify :: k -> Type
type family Starify (a :: k) :: Type where
  Starify (a :: Type) = a
  Starify _           = Void

coerce :: a :== b -> a -> b
coerce f = uncoerce . hsubst f . Coerce