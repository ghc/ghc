{-# LANGUAGE DuplicateRecordFields #-}

module T17469A where

data Foo = MkFoo { foo :: String } deriving Show

data FooWithBar = MkFooWithBar { foo :: String, bar :: Bool } deriving Show

data FooWithBarAndBaz = MkFooWithBarAndBaz { foo :: String, bar :: Bool, baz :: Int } deriving Show
