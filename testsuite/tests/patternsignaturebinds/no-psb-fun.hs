{-# language NoPatternSignatureBinds #-}

module ShouldCompile where

f :: forall a b . a -> b -> b
f (x :: a) =
  undefined
