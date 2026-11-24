{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NativeExprSigWrongReceiver where

import Data.Proxy (Proxy(..))

witness :: foreach (flag :: Bool) -> Proxy flag
witness _ = Proxy

bad :: foreach (x :: Bool) -> foreach (y :: Bool) -> Proxy y
bad x y = witness x :: Proxy y
