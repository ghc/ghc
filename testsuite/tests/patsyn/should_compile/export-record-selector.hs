{-# LANGUAGE PatternSynonyms #-}

module Foo ( A(foo) ) where

data A a = A a

pattern P :: Int -> A Int
pattern P{foo} = A foo
