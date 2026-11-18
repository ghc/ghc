{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedNestedKind where

import qualified Data.Char
import Data.Proxy (Proxy(..))

bad :: Proxy (Data.Char.toUpper (Prelude.reverse '[2, 3, 5]))
bad = Proxy
