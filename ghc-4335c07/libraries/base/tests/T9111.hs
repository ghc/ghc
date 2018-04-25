{-# LANGUAGE DataKinds #-}

module T9111 where

import Data.Typeable

a = typeRep (Proxy :: Proxy 'True)
b = typeRep (Proxy :: Proxy Typeable)
c = typeRep (Proxy :: Proxy (~))
d = typeRep (Proxy :: Proxy 'Left)
