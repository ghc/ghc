{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NativeClosedSaturatedAmbiguous where

import Data.Proxy (Proxy(..))
import Numeric.Natural (Natural)
import qualified NativeClosedSumLib as Lib

bad :: Proxy (Lib.ambiguousPair 18 24 :: Natural)
bad = Proxy
