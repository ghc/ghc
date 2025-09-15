{-# LANGUAGE DataKinds #-}

module T11342c where

import Data.Typeable
import GHC.TypeLits

x :: TypeRep
x = typeRep (Proxy :: Proxy 'x')
