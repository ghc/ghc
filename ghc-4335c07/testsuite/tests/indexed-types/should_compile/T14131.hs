{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T14131 where

import Data.Kind
import Data.Proxy

data family Nat :: k -> k -> *
newtype instance Nat :: (k -> *) -> (k -> *) -> * where
  Nat :: (forall xx. f xx -> g xx) -> Nat f g

type family   F :: Maybe a
type instance F = (Nothing :: Maybe a)

class C k where
  data CD :: k -> k -> *
  type CT :: k

instance C (Maybe a) where
  data CD :: Maybe a -> Maybe a -> * where
    CD :: forall (m :: Maybe a) (n :: Maybe a). Proxy m -> Proxy n -> CD m n
  type CT = (Nothing :: Maybe a)

class Z k where
  type ZT :: Maybe k
  type ZT = (Nothing :: Maybe k)
