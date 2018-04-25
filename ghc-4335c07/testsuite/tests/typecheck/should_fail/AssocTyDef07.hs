{-# LANGUAGE TypeFamilies #-}
module AssocTyDef07 where

class Cls a where
    type Typ a = Int
     -- Default without family
