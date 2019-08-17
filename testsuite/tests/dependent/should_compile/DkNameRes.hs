{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module DkNameRes where

import Data.Proxy
import Data.Kind

type family IfK (e :: Proxy (j :: Bool)) :: Type where
   IfK (_ :: Proxy True) = ()
