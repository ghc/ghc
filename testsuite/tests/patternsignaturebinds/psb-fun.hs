{-# language PatternSignatureBinds #-}

module ShouldCompile where

f :: forall b . b -> b
f (x :: a) =
  undefined
