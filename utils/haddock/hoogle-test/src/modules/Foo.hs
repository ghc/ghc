{-# LANGUAGE Haskell2010 #-}
module Foo where


foo :: Int -> Int
foo = (* 2)


foo' :: Int -> Int -> Int
foo' x y = foo x + foo y
