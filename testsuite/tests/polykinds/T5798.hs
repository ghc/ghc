{-# LANGUAGE PolyKinds, DataKinds #-}

module T5798 where

data Proxy t = ProxyC

test :: Proxy '[Int, Bool]
test = ProxyC
