{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
module T18999_NoFieldSelectors where

data Foo = Foo { bar :: Int, baz :: Int }
baz = 42

foo = Foo { bar = 23, baz = 1 }
y = foo { baz = baz }
