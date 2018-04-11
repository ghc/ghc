{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeInType #-}

module T14066d where

import Data.Proxy

g :: (forall c b (a :: c). (Proxy a, Proxy c, b)) -> ()
g _ = ()

f :: forall b. b -> (Proxy Maybe, ())
f x = (fstOf3 y :: Proxy Maybe, g y)
  where
    y :: (Proxy a, Proxy c, b)  -- this should NOT generalize over b
                                -- meaning the call to g is ill-typed
    y = (Proxy, Proxy, x)

fstOf3 (x, _, _) = x
