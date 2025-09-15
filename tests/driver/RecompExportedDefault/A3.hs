{-# LANGUAGE NamedDefaults #-}
module A (
    Stringify(..),
    default Stringify,
    Bingify(..),
    default Bingify
  ) where

class Stringify a where
  stringify :: a -> String

instance Stringify Int where
  stringify n = "Int"

instance Stringify Bool where
  stringify b = "Bool"

instance Stringify [Char] where
  stringify s = "String"

class Bingify a where
  bingify :: a -> String

instance Bingify Int where
  bingify n = "Int"

instance Bingify Bool where
  bingify b = "Bool"

instance Bingify [Char] where
  bingify s = "String"

-- Switch the order, do not trigger recomp
default Bingify (Int)
default Stringify (Bool, Int)
