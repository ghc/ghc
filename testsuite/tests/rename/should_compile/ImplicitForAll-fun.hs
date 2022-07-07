{-# language NoImplicitForAll, PartialTypeSignatures, NamedWildCards #-}

module ShouldCompile where

h :: forall a . Int -> a -> a
h (i :: b) =
  undefined

f :: forall a . a -> _b -> a
f =
  undefined

g :: forall a _b . a -> _b -> a
g =
  undefined
