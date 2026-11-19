{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module NativeClosedResultDictionary where

import Data.Proxy (Proxy(..))

bad :: Proxy (Prelude.fromIntegral 5 :: Bool)
bad = Proxy
