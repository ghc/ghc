{-# language TypeFamilies #-}

module MultiAssocDefaults where

import Data.Kind (Type)

class C a where
  type A a :: Type
  type A a = Int
  type A a = Double
