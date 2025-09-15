{-# LANGUAGE TypeFamilies #-}

module Kind where

import Data.Kind (Type)

class C (a :: Type -> Type) where
  type T a

foo :: a x -> T a
foo = undefined

