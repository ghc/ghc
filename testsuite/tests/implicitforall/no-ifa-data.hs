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
