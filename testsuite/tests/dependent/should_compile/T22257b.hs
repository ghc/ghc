{-# LANGUAGE TypeFamilies #-}

module T22257b where

import Data.Kind (Type)

type family F (x :: Type) :: Type
type family G (x :: Type) (y :: F x) :: Type

data T
type instance F T = Type
type instance G T k = k
