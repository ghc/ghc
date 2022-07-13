{-# language NoPatternSignatureBinds #-}

module ShouldFail where

f :: forall b . b -> b
f (x :: a) =
  undefined
