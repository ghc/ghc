{-# LANGUAGE TypeFamilyDependencies #-}

module T6018a where

import {-# SOURCE #-} T6018 -- test support for hs-boot files

type family G a b c = (result :: *) | result -> a b c
type instance G Int  Char Bool = Bool
type instance G Char Bool Int  = Int

type instance F Bool Int  Char = Char
