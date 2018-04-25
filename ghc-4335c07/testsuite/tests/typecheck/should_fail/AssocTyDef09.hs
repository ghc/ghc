{-# LANGUAGE TypeFamilies #-}
module AssocTyDef01 where

type family OtherTyp a

class Cls a where
    type Typ a
    type OtherType a = Int
      -- Default for top level AT: want error
