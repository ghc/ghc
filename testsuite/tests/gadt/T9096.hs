{-# LANGUAGE GADTs #-}

module T9096 where

data Foo a where
  MkFoo :: (->) a (Foo a)
