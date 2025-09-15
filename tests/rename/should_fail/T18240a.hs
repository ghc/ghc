{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T18240a where

import Data.Proxy

class C a where
  m :: Proxy a

instance (forall a. C [a]) where
  m = Proxy @[a]

instance (Eq a => C [a]) where
  m = Proxy @[a]

instance (forall a. C (Either a b)) where
  m = Proxy @(Either a b)

instance forall a. (forall b. C (Either a b)) where
  m = Proxy @(Either a b)

instance Eq a => (Eq b => C (Either a b)) where
  m = Proxy @(Either a b)

-- Some other nonsensical instance types

instance 42
instance Int -> Int
