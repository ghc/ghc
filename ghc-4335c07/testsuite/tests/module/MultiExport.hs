{-# LANGUAGE PatternSynonyms #-}
module Foo ( A(x, x) ) where

data A = A  Int

pattern Pattern{x} = A x
