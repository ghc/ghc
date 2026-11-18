{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourceFieldCoverage where

data Nat = Z | S Nat

incomplete :: Nat -> Nat
incomplete Z = Z
incomplete (S Z) = Z

type Answer = incomplete ('S ('S 'Z))
