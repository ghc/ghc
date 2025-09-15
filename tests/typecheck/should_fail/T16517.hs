{-# LANGUAGE PolyKinds #-}
module T16517 where

import Data.Proxy
class C a where m :: Proxy (a :: k)
