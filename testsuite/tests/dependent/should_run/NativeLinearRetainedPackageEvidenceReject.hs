{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageEvidenceReject where
import NativeLinearRetainedPackageProvider

wrong :: OwnedStep a %1 -> (Int, a)
wrong (Step (S _) _ payload a) = (readAt Z payload, a)
wrong (Step Z _ () a) = (0, a)

wrongFlag :: Packet a %1 -> ((Int, Bool), a)
wrongFlag (IndexFirst (S k) True payload flag a) = ((readAt k payload, flag), a)
wrongFlag packet = inspect packet
