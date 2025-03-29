{-# LANGUAGE NamedDefaults #-}
module T25858v1_helper (
    default Semigroup,
    default Stringify,
    Stringify(..),
  ) where

import Data.Semigroup (Sum)

default Semigroup (Sum Integer)

class Stringify a where
  stringify :: a -> String

instance Stringify Int where
  stringify n = "Int: " ++ show n

instance Stringify Bool where
  stringify b = "Bool: " ++ show b

instance Stringify [Char] where
  stringify s = "String: " ++ s

default Stringify (Int)


