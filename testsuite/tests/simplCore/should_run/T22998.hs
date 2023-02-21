{-# LANGUAGE DataKinds #-}
module Main where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (natVal)

main :: IO ()
main = print x
  where
    x = natVal @18446744073709551616 Proxy + natVal @18446744073709551616 Proxy
