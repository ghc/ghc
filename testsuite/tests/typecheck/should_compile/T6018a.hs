{-# LANGUAGE TypeFamilyDependencies #-}

module T6018a where

import Data.Kind (Type)
import {-# SOURCE #-} T6018 -- test support for hs-boot files

type family G a b c = (result :: Type) | result -> a b c
type instance G Int  Char Bool = Bool
type instance G Char Bool Int  = Int

type instance F @Type Bool Int  Char = Char
