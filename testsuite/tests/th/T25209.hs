{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
module T25209 where

import Data.Proxy

$([d| f :: Proxy a -> Proxy a
      f @(a :: k) p = p
    |])
