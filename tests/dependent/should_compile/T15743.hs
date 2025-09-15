{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies #-}

module T15743 where

import Data.Proxy

data T (b :: Proxy (k2 :: k)) c (a :: k)
