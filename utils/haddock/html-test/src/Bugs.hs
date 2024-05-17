{-# LANGUAGE Haskell2010 #-}
module Bugs where

data A a = A a (a -> Int)
