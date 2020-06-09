{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T18240 where

import Data.Proxy

class C a where
  m :: Proxy a

instance (forall a. C [a]) where
  m = Proxy @[a]

instance (Eq a => C [a]) where
  m = Proxy @[a]

instance (forall a. C (Either a b)) where
  m = Proxy @(Either a b)

-- Some other nonseniscal instance types

instance 42
instance Int -> Int
