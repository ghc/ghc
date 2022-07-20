{-# language NoImplicitForAll #-}

module ShouldFail where

f :: a -> ()
f =
  undefined

g :: forall a . a -> b -> ()
g =
  undefined
