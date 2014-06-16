{-# LANGUAGE TypeFamilies #-}
module AssocTyDef01 where

class OtherCls a where
    type OtherTyp a

class Cls a where
    type Typ a
    type OtherType a = Int
      -- Default for another class AT: want error
