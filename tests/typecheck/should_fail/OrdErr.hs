{-# LANGUAGE DataKinds, TypeFamilies #-}
module OrdErr where

import GHC.TypeNats
import Data.Proxy

proxyInEq :: (a <= b) => Proxy a -> Proxy b -> ()
proxyInEq _ _ = ()

-- Throws an error
proxyInEq1 :: Proxy a -> Proxy (a+1) -> ()
proxyInEq1 = proxyInEq

-- Throws an error
proxyInEq2 :: Proxy 5 -> Proxy 3 -> ()
proxyInEq2 = proxyInEq

-- No error
proxyInEq3 :: Proxy 3 -> Proxy 4 -> ()
proxyInEq3 = proxyInEq

-- No error
proxyInEq4 :: (a <=? (a + 1)) ~ True => Proxy a -> Proxy (a + 1) -> ()
proxyInEq4 = proxyInEq

-- No error
proxyInEq5 :: (a <= (a + 1)) => Proxy a -> Proxy (a + 1) -> ()
proxyInEq5 = proxyInEq

-- No error
proxyInEq6 :: (CmpNat a (a + 1) ~ EQ) => Proxy a -> Proxy (a + 1) -> ()
proxyInEq6 = proxyInEq

-- No error
proxyInEq7 :: (CmpNat a (a + 1) ~ LT) => Proxy a -> Proxy (a + 1) -> ()
proxyInEq7 = proxyInEq
