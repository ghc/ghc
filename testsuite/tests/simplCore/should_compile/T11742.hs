{-# LANGUAGE Strict #-}

module Foo where

data Foo = Foo

instance Eq Foo where
  (==) Foo Foo = True
