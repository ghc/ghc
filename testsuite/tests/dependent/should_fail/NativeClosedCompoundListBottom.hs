{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedCompoundListBottom where

import Data.Proxy (Proxy(..))
import qualified NativeClosedCompoundListsLib as P

bad :: Proxy (P.bottomElement 1)
bad = Proxy
