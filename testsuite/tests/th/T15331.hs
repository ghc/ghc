{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module T15331 where

import Data.Proxy

$([d| f :: Proxy (Int -> Int)
      f = Proxy @(Int -> Int)
    |])
