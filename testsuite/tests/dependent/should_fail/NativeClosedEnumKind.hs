{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumKind where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A
import qualified NativeClosedEnumB as B

bad :: Proxy (A.score (B.choose 0))
bad = Proxy
