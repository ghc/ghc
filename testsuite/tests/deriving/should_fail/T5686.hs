{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module Main where

data U a = U (G a) deriving Functor

class A a where
   type G a
