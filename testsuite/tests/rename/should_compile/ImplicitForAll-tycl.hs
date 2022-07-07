{-# language NoImplicitForAll, TypeFamilies #-}

module ShouldCompile where

import Data.Kind (Type)

data family DF a :: a

type D :: forall k . k -> Type
data D a

data DK (a :: k)

type F2 :: forall k . Type -> k -> Type
type family F2 a

type instance F2 Int = D

data E where
  E :: forall a . a -> (a -> Int) -> E

class C1 k a where
  type Assoc1 a :: k -> Type

class C2 (a :: k2) where
  type Assoc2 a :: forall k . k -> Type

data family DF2 :: forall k . k -> Type

data instance DF2 :: forall k . (k -> Type) -> Type where
  DF2 :: forall f . (forall x. x -> f x) -> DF2 f

class C3 a

instance forall a . C3 a => C3 (Maybe a)
