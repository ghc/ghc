-- !!! newtypes with a labelled field.
module ShouldCompile where

newtype Foo = Foo { x :: Int } deriving (Eq)

f :: Foo -> Foo -> Int
f a b = x a + x b
