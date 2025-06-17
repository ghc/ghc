{-# LANGUAGE GHC2021 #-}
module Main where
import qualified T13847A as A
foo = "foo"
main = print $ A.foo $ A.A { foo = () }
