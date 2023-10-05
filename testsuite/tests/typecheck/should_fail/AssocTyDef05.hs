{-# LANGUAGE TypeFamilies #-}
module AssocTyDef05 where

class Cls a where
    type Typ a
    type Typ = Maybe
      -- Too few params for default