{-# language NoImplicitForAll #-}

module ShouldFail where

f :: a -> b -> b
f =
  undefined

g :: forall b . a -> b -> b
g =
  undefined
