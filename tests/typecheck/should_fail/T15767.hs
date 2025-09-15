{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}

module T15767 where

class C a b | b -> a where f :: a -> b

y = x where
  x :: (C () b, C Bool b) => b
  x  = f ()
