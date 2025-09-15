{-# LANGUAGE PolyKinds, DataKinds #-}
module T15116a where

import Data.Proxy

data B = MkB (Proxy 'MkB)
