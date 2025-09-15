{-# LANGUAGE TypeFamilies #-}
module AssocTyDef03 where

class Cls a where
    data Typ a
    type Typ a = Int
      -- Default for data family :-(