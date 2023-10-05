{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeOperators #-}
module Foo where

class (a ~ b) => C a b where
  op :: a -> a -> b

f :: C a b => a -> b
f x = op x x
