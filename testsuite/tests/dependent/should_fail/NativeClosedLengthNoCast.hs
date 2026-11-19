{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NativeClosedLengthNoCast where

import Data.Proxy (Proxy(..))
import GHC.TypeNats (Nat)

bad :: Proxy (Prelude.length '[1, 2, 3] :: Nat)
bad = Proxy
