{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedIndexHidden where

import Data.Proxy
import qualified NativeClosedIndexFailures as P

bad :: Proxy (P.hidden 1)
bad = Proxy
