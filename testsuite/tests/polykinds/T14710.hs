{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T14710 where

import Data.Proxy

class C a b where
  c1 :: Proxy (x :: a) -> b
  c2 :: forall (x :: a). Proxy x -> b

f :: forall a. a -> a
f x = const x (const g1 g2)
  where
    g1 :: Proxy (x :: a)
    g1 = Proxy

    g2 :: forall (x :: a). Proxy x
    g2 = Proxy

h1 :: forall k a. Proxy (a :: k)
h1 = Proxy

h2 :: forall k (a :: k). Proxy a
h2 = Proxy
