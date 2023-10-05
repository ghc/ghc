{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.IORef
import Data.Kind (Type)

data family T a
data instance T a = T

foo :: T Int -> T Char
foo T = T

type family S a
type instance S a = a

type family SMRef (m:: Type -> Type) :: Type -> Type
type instance SMRef IO = IORef
