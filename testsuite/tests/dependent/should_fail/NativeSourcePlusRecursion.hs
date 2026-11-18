{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourcePlusRecursion where

data Nat = Z | S Nat

swap :: Nat -> Nat -> Nat
swap Z     m = m
swap (S n) m = S (swap m n)

type Swapped = swap ('S 'Z) 'Z
