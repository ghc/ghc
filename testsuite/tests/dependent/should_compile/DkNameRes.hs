{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, StandaloneKindSignatures #-}

module DkNameRes where

import Data.Proxy
import Data.Kind

type IfK :: Proxy (j :: Bool) -> Type
type family IfK e where
   IfK (_ :: Proxy True) = ()
