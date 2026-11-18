{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourceLengthRecursion where

data Nat = Z | S Nat

loop :: [Bool] -> Nat
loop [] = Z
loop (b : bs) = S (loop (b : bs))

type Answer = loop '[ 'True]
