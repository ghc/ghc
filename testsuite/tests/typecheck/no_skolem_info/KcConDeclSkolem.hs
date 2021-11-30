{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}

module KcConDeclSkolem where

import Data.Kind
import Data.Proxy

data G a where
  D :: Proxy (a :: k) -> Proxy (b :: k) -> G (a b)
