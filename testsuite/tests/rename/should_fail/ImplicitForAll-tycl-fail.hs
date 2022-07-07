{-# language NoImplicitForAll, TypeFamilies #-}

module ShouldFail where

import Data.Kind (Type)

data Imp1 a :: b -> Type

data Imp2 a :: forall b . b -> c -> Type

type family F a :: b

data family DF a :: a

data instance DF (Maybe a) :: Maybe a

data E where
  E :: a -> (a -> Int) -> E

class C a where
  type AssocC a :: k -> Type

class D a where
  type AssocD a :: Type -> Type

data family DF2 :: k -> Type

data instance DF2 :: (k -> Type) -> Type where
  DF2 :: (forall x. x -> f x) -> DF2 f

class C3 a

instance C3 a => C3 (Maybe a)
