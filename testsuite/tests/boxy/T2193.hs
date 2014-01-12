{-# OPTIONS_GHC -XImpredicativeTypes -fno-warn-deprecated-flags #-}

module Main where

data Foo x y = Foo {foo1 :: x, foo2 :: y}
instance Functor (Foo x) where
    fmap f (Foo x y) = Foo x (f y)

bar :: a -> Foo (forall s. s) a
bar a = Foo undefined a

main = print (foo2 (fmap (*2) (bar 2)))

