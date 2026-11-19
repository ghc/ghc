{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumField where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A

bad :: Proxy (A.unboxedValue 0)
bad = Proxy
