{-# LANGUAGE TypeFamilies #-}
module T16116b where

class C a where
  type F a
instance C [a] where
  type F [a] = b
