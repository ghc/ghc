{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedConstructorEvidence where

import Data.Proxy (Proxy(..))
import qualified NativeClosedCompoundListsLib as P

bad :: Proxy (P.needsEvidence (P.explode 0) ('Right 'x') 'True)
bad = Proxy
