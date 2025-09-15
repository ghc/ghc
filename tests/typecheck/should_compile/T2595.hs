{-# LANGUAGE Rank2Types #-}
module Foo where

data Foo = forall a . Foo { foo :: a -> a, bar :: Int }

x :: Foo
x = Foo { foo = id, bar = 3 }

f :: Foo -> Foo
f rec = rec { foo = id }

g :: Foo -> Foo
g rec = rec { bar = 3 }
