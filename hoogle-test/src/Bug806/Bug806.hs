{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug806 where

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
