{-# LANGUAGE NoFieldSelectors #-}

module T23557_aux where

foo :: Int
foo = 23

data Foo = Foo {
  foo :: Int
}
