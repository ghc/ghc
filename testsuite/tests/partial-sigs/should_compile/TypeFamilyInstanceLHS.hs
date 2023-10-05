{-# LANGUAGE TypeFamilies #-}
module TypeFamilyInstanceLHS where

import Data.Kind (Type)

type family F (a :: Type) (b :: Type) :: Type
type instance F Int  _ = Int
type instance F Bool _ = Bool

foo :: F Int Char -> Int
foo = id
