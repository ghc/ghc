{-# OPTIONS_GHC -Werror #-}

module T3966 where

data Foo a b = Foo {-# UNPACK #-} !(a -> b)
