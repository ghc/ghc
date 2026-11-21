{-# LANGUAGE ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeLinearRetainedPackageFallthroughReject where
import NativeLinearRetainedPackageProvider

readAgain :: Triple a %1 -> (Int, a)
readAgain (Three Z () _ False _ _ _ a) = (0, a)
readAgain (Three (S n) payload True True _ True _ a) = (readAt n payload, a)
readAgain (Three (S n) payload True True _ False _ a) = (readAt n payload, a)
readAgain pkg = chooseThree pkg
