{-# LANGUAGE TypeFamilyDependencies #-}

module T6018Afail where

import Data.Kind (Type)

type family G a b c = (result :: Type) | result -> a b c
type instance G Int  Char Bool = Bool
type instance G Char Bool Int  = Int
