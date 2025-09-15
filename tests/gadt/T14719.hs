{-# LANGUAGE GADTs #-}
module T14719 where

data Foo1 where
  MkFoo1 :: Bool

newtype Foo2 where
  MkFoo2 :: Foo2
