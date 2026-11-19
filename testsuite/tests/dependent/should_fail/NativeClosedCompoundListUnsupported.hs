{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}

module NativeClosedCompoundListUnsupported where

import Data.Proxy (Proxy(..))
import qualified NativeClosedCompoundListsLib as P

bad :: Proxy (P.unsupported 1)
bad = Proxy
