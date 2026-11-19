{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedConstructorAmbiguous where

import qualified NativeClosedCompoundListsLib as P
import Data.Proxy (Proxy(..))

bad :: Proxy (P.ambiguous 'Nothing)
bad = Proxy
