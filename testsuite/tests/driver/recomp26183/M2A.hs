{-# LANGUAGE TypeFamilies #-}
module M2 where

class C a where
  type AT a

instance C () where
  type AT () = Bool
