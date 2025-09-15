{-# LANGUAGE TypeFamilies #-}

module T12088i where

import Data.Kind (Type)

type family K t :: Type
type family T t :: K t -> Type

data List

type instance K List = Type
type instance T List = []

