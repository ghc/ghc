{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
module T13781 where

import Data.Kind
import Data.Proxy

$([d| f :: Proxy (a :: (k :: Type))
      f = Proxy
    |])
