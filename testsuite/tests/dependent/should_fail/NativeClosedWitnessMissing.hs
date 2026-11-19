{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedWitnessMissing where

import Data.Proxy
import IndexedVec
import qualified NativeClosedWitnessFailures as P

bad :: Proxy (P.noWitness 4)
bad = Proxy
