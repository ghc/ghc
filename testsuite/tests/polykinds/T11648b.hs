{-# LANGUAGE TypeInType #-}

module T11648b where

import Data.Proxy

data X (a :: Proxy k)
