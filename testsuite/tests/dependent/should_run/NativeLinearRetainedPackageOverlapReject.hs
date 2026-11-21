{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageOverlapReject where
import NativeLinearRetainedPackageProvider (Nat(..))

data Packet a where
    Packet :: foreach (n :: Nat) -> foreach (b :: Bool) -> Bool -> a %1 -> Packet a

consume :: Packet a %1 -> a
consume (Packet (S _) True True a) = a
consume (Packet (S _) True False a) = a
consume (Packet (S _) False _ a) = a
consume (Packet Z _ _ a) = a
