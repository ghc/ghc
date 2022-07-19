{-# language NoImplicitForAll, TypeFamilies #-}

module ShouldFail where

import Data.Kind (Type)

data Imp1 a :: b -> Type

data Imp2 a :: forall b . b -> c -> Type

type family F a :: b

data family DF a :: a

data instance DF (Maybe a) :: Maybe a
