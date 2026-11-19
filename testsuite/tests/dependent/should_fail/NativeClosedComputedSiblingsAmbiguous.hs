{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedComputedSiblingsAmbiguous where

import Data.Proxy (Proxy(..))

bad :: Proxy (Prelude.max (Prelude.fromIntegral (Prelude.length '[1]))
                          (Prelude.fromIntegral (Prelude.length '[2])))
bad = Proxy
