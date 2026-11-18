{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSourceMixed where

import Data.Proxy (Proxy(..))
import Prelude

bad :: Proxy (Prelude.reverse '[5, 'False])
bad = Proxy
