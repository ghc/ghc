{-# LANGUAGE PatternSynonyms #-}

module Foo (A(B, C)) where

data A a = A

pattern B :: A Int
pattern B = A

pattern C :: A String
pattern C = A
