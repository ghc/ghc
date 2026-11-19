{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedIndexFamily where

import Data.Proxy
import qualified NativeClosedIndexFailures as P

bad :: Proxy (P.blocked 1)
bad = Proxy
