{-# LANGUAGE PolyKinds, DataKinds, MultiParamTypeClasses,
              FunctionalDependencies
  #-}

module T7128 where

class Foo a (b :: k) | a -> k
instance Foo Bool False
