{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumPartial where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A (Token(Ready), choose)

bad :: Proxy (A.choose 0)
bad = Proxy
