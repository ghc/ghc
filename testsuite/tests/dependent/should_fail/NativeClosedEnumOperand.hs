{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumOperand where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A (Token(Ready), score)

bad :: Proxy (A.score 'A.Ready)
bad = Proxy
