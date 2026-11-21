{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageEvidenceReject where
import NativeLinearRetainedPackageProvider

wrong :: OwnedStep a %1 -> (Int, a)
wrong (Step (S _) _ payload a) = (readAt Z payload, a)
wrong (Step Z _ () a) = (0, a)
