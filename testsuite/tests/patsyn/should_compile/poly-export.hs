{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Foo (Foo(P)) where

data Foo a = Foo a

instance C Foo where
  build a = Foo a
  destruct (Foo a) = a

class C f where
  build :: a -> f a
  destruct :: f a -> a

pattern P :: C f => a -> f a
pattern P x <- (destruct -> x)
  where
        P x = build x
