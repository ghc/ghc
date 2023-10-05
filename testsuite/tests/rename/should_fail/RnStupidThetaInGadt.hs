{-# LANGUAGE DatatypeContexts #-}
module RnStupidThetaInGadt where


data (Eq a) => Foo a where
  MkFoo :: Int
