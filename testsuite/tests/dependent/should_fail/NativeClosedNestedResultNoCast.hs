{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedNestedResultNoCast where

import Data.Proxy (Proxy(..))
import qualified NativeClosedSumLib as P

bad :: Proxy (P.keepNatural (Prelude.length '[1, 2, 3]))
bad = Proxy
