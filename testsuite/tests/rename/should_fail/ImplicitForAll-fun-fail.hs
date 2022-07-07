{-# language NoImplicitForAll #-}

module ShouldFail where

f :: a -> b -> b
f =
  undefined

g :: forall b . a -> b -> b
g =
  undefined

l :: forall a . a -> a
l =
  undefined
  where
    lc :: a -> b
    lc =
      undefined
