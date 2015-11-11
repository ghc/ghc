{-# LANGUAGE PatternSynonyms #-}
module Foo () where

data A a = A a

pattern Q :: () => (A ~ f) => a -> f a
pattern Q a = A a
