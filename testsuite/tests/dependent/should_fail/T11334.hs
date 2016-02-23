{-# LANGUAGE DataKinds, NoPolyKinds #-}

module T11334 where

import Data.Functor.Compose
import Data.Proxy

p = Proxy :: Proxy 'Compose
