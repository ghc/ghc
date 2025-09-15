{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}

module KcConDeclSkolem2 where

import Data.Kind
import Data.Proxy

data D a = MkD (a a)
