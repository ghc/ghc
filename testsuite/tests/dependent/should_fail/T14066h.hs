{-# LANGUAGE ScopedTypeVariables, TypeInType, MonoLocalBinds #-}

module T14066h where

import Data.Proxy

f :: forall b. b -> (Proxy Int, Proxy Maybe)
f x = (fst y :: Proxy Int, fst y :: Proxy Maybe)
  where
    y :: (Proxy a, b)  -- MonoLocalBinds means this won't generalize over the kind of a
    y = (Proxy, x)
