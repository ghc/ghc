{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedNestedResultAmbiguous where

import Data.Proxy (Proxy(..))

bad :: Proxy (Prelude.odd (Prelude.fromIntegral (Prelude.length '[1, 2, 3])))
bad = Proxy
