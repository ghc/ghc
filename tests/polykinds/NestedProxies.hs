{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StarIsType #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
-- NB: -XNoPolyKinds. All the variables in the Proxies should be defaulted to *.

module NestedProxies where

import Data.Proxy

-- | 'F1' docs
type family F1 a b :: * -> *
-- | 'F2' docs
type family F2 a b :: * -> * where
  F2 Int b = Maybe
  F2 a   b = []
-- | 'D' docs
data family D a :: * -> *

v :: Int
v = 42

-- | 'C' docs
class C a where
  -- | 'AT' docs
  type AT a
  type AT a = Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy)))))))))
