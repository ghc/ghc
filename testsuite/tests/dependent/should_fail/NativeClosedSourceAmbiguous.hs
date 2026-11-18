{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NativeClosedSourceAmbiguous where

import Data.Proxy (Proxy(..))
import Numeric.Natural (Natural)
import Prelude

bad :: Proxy (Prelude.fromIntegral 5 :: Natural)
bad = Proxy
