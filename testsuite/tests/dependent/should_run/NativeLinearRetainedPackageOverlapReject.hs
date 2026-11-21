{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageOverlapReject where

data Nat = Z | S Nat

data OwnedStep a where
    Step :: foreach (n :: Nat) -> Bool -> a %1 -> OwnedStep a

takeStep :: OwnedStep a %1 -> a
takeStep (Step (S _) True a) = a
takeStep (Step (S _) False a) = a
takeStep (Step Z _ a) = a
