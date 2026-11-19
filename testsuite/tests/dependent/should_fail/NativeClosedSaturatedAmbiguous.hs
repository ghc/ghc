{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSaturatedAmbiguous where

import Data.Proxy (Proxy(..))
import qualified NativeClosedSumLib as Lib

bad :: Proxy (Lib.ambiguousPair 18 24)
bad = Proxy
