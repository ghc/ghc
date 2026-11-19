{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSaturatedUnder where

import Data.Proxy (Proxy(..))

bad :: Proxy (Prelude.gcd 18)
bad = Proxy
