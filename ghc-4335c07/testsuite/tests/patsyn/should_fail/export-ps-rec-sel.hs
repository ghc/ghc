{-# LANGUAGE PatternSynonyms #-}
module Foo( R(P,x)) where

data Q = Q Int

data R = R

pattern P{x} = Q x
