{-# LANGUAGE TypeInType #-}

module T14066f where

import Data.Proxy

-- a can't come before k.
type P a k = Proxy (a :: k)
