{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedExpanding where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A (Growth(..), expanding)

bad :: Proxy (A.expanding 0)
bad = Proxy
