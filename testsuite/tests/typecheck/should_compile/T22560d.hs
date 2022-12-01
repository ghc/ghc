{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CUSKs #-}

module T22560d where

import Data.Kind
import Data.Proxy

-- type B :: forall k. k -> forall j. j -> Type
data B @(k :: Type) (a :: k) @(j :: Type) (b :: j)

-- type D :: forall k. k -> Type
data D @(k :: Type) (a :: k) = MkD (Proxy k) (Proxy a)

-- type N :: forall k. k -> Type
newtype N @(k :: Type) (a :: k) = MkN (Proxy a -> Proxy k)

-- type S :: forall k. k -> Type
type S @(k :: Type) (a :: k) = Proxy a -> Proxy k :: Type

-- type C :: forall k. k -> Constraint
class C @(k :: Type) (a :: k) where
  f :: Proxy a -> Proxy k

-- type F :: forall k. k -> k
type family F @(k :: Type) (a :: k) :: k where
  F a = a

-- type U :: forall k (a :: k). Type
type U @(a :: k) = Int :: Type

-- type Z :: forall k -> forall (a :: k). Type
type Z (k :: Type) @(a :: k) = Proxy a :: Type