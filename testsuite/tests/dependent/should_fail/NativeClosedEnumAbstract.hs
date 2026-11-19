{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedEnumAbstract where

import Data.Proxy (Proxy(..))
import qualified NativeClosedEnumA as A

bad :: Proxy (A.abstractValue 0)
bad = Proxy
