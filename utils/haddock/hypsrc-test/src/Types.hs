{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}


module Types where


data Quux = Bar | Baz

newtype Foo = Foo ()

type FooQuux = (Foo, Quux)
type QuuxFoo = (Quux, Foo)


data family Norf a b

data instance Norf Foo Quux = NFQ Foo Quux
data instance Norf Quux Foo = NQF Quux Foo


type family Norf' a b

type instance Norf' Foo Quux = (Foo, Quux)
type instance Norf' Quux Foo = (Quux, Foo)


norf1 :: Norf Foo Quux -> Int
norf1 (NFQ (Foo ()) Bar) = 0
norf1 (NFQ (Foo ()) Baz) = 1

norf2 :: Norf Quux Foo -> Int
norf2 (NQF Bar (Foo ())) = 0
norf2 (NQF Baz (Foo ())) = 1


norf1' :: Norf' Foo Quux -> Int
norf1' (Foo (), Bar) = 0
norf1' (Foo (), Baz) = 1

norf2' :: Norf' Quux Foo -> Int
norf2' (Bar, Foo ()) = 0
norf2' (Baz, Foo ()) = 1
