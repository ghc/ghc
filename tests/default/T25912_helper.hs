{-# LANGUAGE NamedDefaults #-}

module T25912_helper ( default C, C(c), default B, b ) where

class C a where
  c :: a
instance C Int where
  c = 1
instance C String where
  c = "String"
default C (String)

class B a where
  b :: a
instance B String where
  b = "String"
default B (String)
