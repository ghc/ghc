{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds #-}

module T10713 where

import Data.Proxy

type family TEq t s where
  TEq t t = 'True
  TEq t s = 'False
data family T a

foo :: Proxy (TEq (T Int) (T Bool)) -> Proxy 'False
foo = id
