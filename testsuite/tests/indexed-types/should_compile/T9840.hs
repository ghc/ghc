{-# LANGUAGE TypeFamilies #-}

module T9840 where

import Data.Kind (Type)
import T9840a

type family X :: Type -> Type where

type family F (a :: Type -> Type) where

foo :: G (F X) -> G (F X)
foo x = x
