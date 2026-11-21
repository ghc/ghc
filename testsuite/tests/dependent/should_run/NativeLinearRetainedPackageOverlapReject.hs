{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageOverlapReject where
import NativeLinearRetainedPackageProvider (Flag, Nat(..))

data Mark where
    Mark :: foreach (b :: Bool) -> Flag b -> Mark

data Packet a where
    Packet :: foreach (n :: Nat) -> Bool -> foreach (m :: Mark) -> Bool -> a %1 -> Packet a

consume :: Packet a %1 -> a
consume (Packet Z _ _ _ a) = a
consume (Packet (S _) _ (Mark b payload) True a) = a
consume (Packet (S _) _ (Mark c payload) False a) = a
