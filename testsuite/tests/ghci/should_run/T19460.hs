module T19460 (Foo(..)) where

class Foo a where
  bar :: a -> ()
  bar _ = ()
