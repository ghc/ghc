{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module T816 where

class Foo x y | x -> y where
  foo :: x -> y

class Bar x y where
  bar :: x -> y -> Int

instance (Foo x y, Bar y z) => Bar x z where
  bar x z = bar (foo x) z
