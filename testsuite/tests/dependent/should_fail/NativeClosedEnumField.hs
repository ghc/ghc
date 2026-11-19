{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumField where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A

bad :: Proxy (A.strictValue 0)
bad = Proxy
