{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeRetainedSecondColumn where

data Nat = Z | S Nat
data Seen (n :: Nat) (b :: Bool) = Seen

missing :: foreach (n :: Nat) -> foreach (b :: Bool) -> Seen n b
missing Z _ = Seen
missing (S _) False = Seen
