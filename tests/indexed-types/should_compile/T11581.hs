{-# LANGUAGE TypeFamilies #-}

module T11581 where

import Data.Kind (Type)

type family F a :: Type -> Type
type family G a

type instance G [a] = F a (Int,Bool)
