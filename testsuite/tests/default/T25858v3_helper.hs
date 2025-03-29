{-# LANGUAGE NamedDefaults #-}

module T25858v3_helper (
    default Show,
    Foo(..),
  ) where

data Foo = Foo deriving (Read)
instance Show Foo where
  show _ = "Foo"

default Show (Foo)
