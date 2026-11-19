{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedWitnessMismatch where

import Data.Proxy
import ExistentialVec
import IndexedVec
import qualified NativeClosedWitnessFailures as P

bad :: Proxy (P.misaligned 4)
bad = Proxy
