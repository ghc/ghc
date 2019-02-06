{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module T14579b where

import Data.Kind
import Data.Proxy

-- type P :: forall {k} {t :: k}. Proxy t
type P = 'Proxy

-- type Wat :: forall a. Proxy a -> *
newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a)
  deriving Eq

-- type Wat2 :: forall {a}. Proxy a -> *
type Wat2 = Wat

-- type Glurp :: * -> *
newtype Glurp a = MkGlurp (Wat2 (P :: Proxy a))
  deriving Eq
