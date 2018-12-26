{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

module T15798c () where

data Foo a = Foo a
  deriving (Eq)
