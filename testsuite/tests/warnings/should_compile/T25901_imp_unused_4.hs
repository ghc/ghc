{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

module T25901_imp_unused_4 where

import GHC.TypeLits (symbolVal)
import Data.Proxy (type .., data Proxy, asProxyTypeOf)
    -- the wildcard and `asProxyTypeOf` are unused

s :: String
s = symbolVal (Proxy @"hello")