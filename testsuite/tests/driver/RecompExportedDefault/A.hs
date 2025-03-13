{-# LANGUAGE NamedDefaults #-}
module A (
    Stringify(..),
    default Stringify
  ) where

class Stringify a where
  stringify :: a -> String

instance Stringify Int where
  stringify n = "Int"

instance Stringify Bool where
  stringify b = "Bool"

instance Stringify [Char] where
  stringify s = "String"

default Stringify (Int)
