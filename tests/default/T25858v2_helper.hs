{-# LANGUAGE NamedDefaults #-}

module T25858v2_helper ( default C, c, default B, b ) where

class C a where
  c :: a
instance C Int where
  c = 1
instance C String where
  c = "String"
default C (String)

class B a where
  b :: a
instance B Int where
  b = 1
instance B String where
  b = "String"
default B (Int)
