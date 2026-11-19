{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSaturatedOver where

import Data.Proxy (Proxy(..))

bad :: Proxy (Prelude.gcd 18 24 36)
bad = Proxy
