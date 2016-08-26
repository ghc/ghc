{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}

module T12533 where

class Foo x where
  foo :: forall a . x a -> x a
  default foo :: forall a . x a -> x a
  foo x = go
    where go :: x a
          go = undefined
