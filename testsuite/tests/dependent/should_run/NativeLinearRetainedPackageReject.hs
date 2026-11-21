{-# LANGUAGE GADTs, LinearTypes, NoExperimentalDependentTypes #-}
module NativeLinearRetainedPackageReject where
import NativeLinearRetainedPackageProvider

duplicate :: Slot a %1 -> (a, a)
duplicate (Slot _ _ a) = (a, a)
discard :: Slot a %1 -> ()
discard (Slot _ _ _) = ()
