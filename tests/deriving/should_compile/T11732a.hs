{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module T11732a where

import GHC.Generics

data Proxy k (a :: k) deriving Generic1

data family ProxyFam (a :: y) (b :: z)
data instance ProxyFam k (a :: k) deriving Generic1
