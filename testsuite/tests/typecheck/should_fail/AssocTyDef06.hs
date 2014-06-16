{-# LANGUAGE TypeFamilies #-}
module AssocTyDef06 where

class Cls a where
    type Typ a
    type Typ a Int = Int
      -- Too many params for default