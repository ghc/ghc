{-# language ImplicitForAll #-}

module ShouldFail where

g :: forall b . a -> b -> b
g =
  undefined
