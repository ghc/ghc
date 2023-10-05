{-# LANGUAGE Haskell2010 #-}
module NoNullaryTC where

class A where
  f :: a -> a
