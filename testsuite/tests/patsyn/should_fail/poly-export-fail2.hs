{-# LANGUAGE PatternSynonyms #-}
module Foo (A(P)) where

data A = A

data B = B

pattern P :: () => (f ~ B) => f
pattern P = B
