{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module NativeClosedSumDependencies where

import qualified NativeClosedSumDependenciesInstance
import Numeric.Natural (Natural)

data NatTag (n :: Natural) where
    Ten :: NatTag 10

answer :: NatTag (NativeClosedSumDependenciesInstance.pick '[2, 3, 5])
answer = Ten
