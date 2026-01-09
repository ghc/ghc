{-# LANGUAGE ImpredicativeTypes, ImplicitParams #-}

module T26737 where

import Data.Coerce

newtype Foo = MkFoo Int

b :: ((?foo :: Foo) => Int) -> ((?foo :: Int) => Int)
b = coerce @(((?foo :: Foo) => Int)) @(((?foo :: Int) => Int))
