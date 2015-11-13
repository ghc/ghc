{-# LANGUAGE PatternSynonyms #-}

module Foo (MyClass(.., P)) where

pattern P = Nothing

class MyClass a where
  foo :: a -> Int
