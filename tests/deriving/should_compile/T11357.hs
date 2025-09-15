{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T11357 where

import GHC.Generics (Generic1)

data family   ProxyFam (a :: k)
data instance ProxyFam (a :: k) = ProxyCon deriving Generic1
