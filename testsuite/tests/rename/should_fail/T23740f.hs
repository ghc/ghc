{-# LANGUAGE RequiredTypeArguments #-}

module T23740f where

import Data.Proxy

p :: () -> forall (a :: id). Proxy a
p () = Proxy