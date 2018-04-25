{-# LANGUAGE TypeFamilies #-}
module AssocTyDef02 where

class Cls a where
    type Typ a
    type Typ [b] = Int
      -- Default is not parametric in type class params

