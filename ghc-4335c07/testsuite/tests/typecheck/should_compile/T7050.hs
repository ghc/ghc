module T7050 where

data Foo a = Foo {-# UNPACK #-} !(Foo a) 
