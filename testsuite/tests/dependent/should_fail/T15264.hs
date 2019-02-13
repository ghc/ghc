{-# LANGUAGE ExplicitForAll, PolyKinds #-}

module T15264 where

import Data.Proxy

bad1 :: forall (a :: k). Proxy a -> ()
bad1 _ = ()

bad2 :: forall (a :: k1) (b :: k2). Proxy a -> ()
bad2 _ = ()

good :: forall k (a :: k). Proxy a -> ()
good _ = ()
