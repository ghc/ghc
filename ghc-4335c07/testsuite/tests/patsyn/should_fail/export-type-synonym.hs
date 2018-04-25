{-# LANGUAGE PatternSynonyms #-}

module Foo ( A(P) ) where

data A = A
data B = B

type C = B

pattern P :: C
pattern P = B
