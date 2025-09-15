{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T17710 where

import Data.Proxy

foo :: forall k (a :: k) (b :: k). Proxy a -> Proxy b
foo x = Proxy
{-# INLINABLE [1] foo #-}
{-# RULES "foo" forall (x :: Proxy (a :: k)). foo x = Proxy #-}

bar :: forall k (b :: k). (forall (a :: k). Proxy a -> Proxy a) -> Proxy b
bar g = g Proxy
{-# INLINABLE [1] bar #-}
{-# RULES "bar" forall (g :: forall (a :: k). Proxy a -> Proxy a). bar g = g Proxy #-}
