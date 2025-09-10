{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module T26396 where

import Data.Kind
import Data.Type.Equality
import GHC.TypeNats

newtype Vector (el :: Type) (len :: Natural) = Vector [el]
  deriving (Eq)

data Sized (f :: Natural -> Type) where
  Sized :: KnownNat len => f len -> Sized f

instance (forall (len :: Natural). Eq (f len)) => Eq (Sized f) where
  Sized xs == Sized ys = case sameNat xs ys of
    Nothing -> False
    Just Refl -> xs == ys

newtype Foo (el :: Type) = Foo (Sized (Vector el))
  deriving (Eq)
