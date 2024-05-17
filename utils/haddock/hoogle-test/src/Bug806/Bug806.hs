{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug806 where

import Data.Kind (Type)
import Data.Proxy

-- | 'F1' docs
type family F1 a b :: Type -> Type
-- | 'F2' docs
type family F2 a b :: Type -> Type where
  F2 Int b = Maybe
  F2 a   b = []
-- | 'D' docs
data family D a :: Type -> Type

v :: Int
v = 42

-- | 'C' docs
class C a where
  -- | 'AT' docs
  type AT a
  type AT a = Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy)))))))))
