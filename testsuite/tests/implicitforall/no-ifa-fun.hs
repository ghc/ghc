{-# language NoImplicitForAll #-}

module ShouldCompile where

h :: forall a . Int -> a -> a
h (i :: b) =
  undefined
