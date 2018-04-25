{-# LANGUAGE TypeFamilies #-}
module AssocTyDef04 where

class Cls a where
    type Typ a
    type Typ a = Maybe
      -- Wrong kind for default