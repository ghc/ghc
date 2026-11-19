{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSourceAmbiguous where

import Data.Proxy (Proxy(..))
import Prelude

bad :: Proxy (Prelude.fromIntegral (Prelude.length '[1,2,3]))
bad = Proxy
