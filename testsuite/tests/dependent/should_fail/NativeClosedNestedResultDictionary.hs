{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module NativeClosedNestedResultDictionary where

import Data.Proxy (Proxy(..))
import qualified NativeClosedSumLib as P

bad :: Proxy (P.ignoreBool (Prelude.fromIntegral (P.bottomSigned 1)))
bad = Proxy
