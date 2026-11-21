{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageOverlapReject where
import NativeLinearRetainedPackageProvider (Nat(..))

data Pair a where
    Pair :: foreach (n :: Nat) -> foreach (b :: Bool) -> a %1 -> Pair a

takePair :: Pair a %1 -> a
takePair (Pair (S _) True a) = a
takePair (Pair (S _) False a) = a
takePair (Pair Z _ a) = a
