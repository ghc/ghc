{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumOperand where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A (Abstract, Hidden, scoreAbstract)

bad :: Proxy (A.scoreAbstract A.Hidden)
bad = Proxy
