{-# LANGUAGE TypeFamilyDependencies #-}

module T6018Afail where

type family G a b c = (result :: *) | result -> a b c
type instance G Int  Char Bool = Bool
type instance G Char Bool Int  = Int
