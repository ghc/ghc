module T5366 where

newtype Id a = Id Int
data Foo = Foo {-# UNPACK #-} !(Id Foo) String 
data Bar = Bar {-# UNPACK #-} !Foo

f :: Bar -> Int
f (Bar (Foo (Id x) _)) = x
