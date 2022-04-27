{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T15453 where

import Data.Kind
import Data.Proxy
import Data.Type.Equality

type family S :: Type where
  S = T
type family T :: Type where
  T = Int

f :: (forall (x :: S). Proxy x) :~: (forall (x :: T). Proxy x)
f = Refl

g :: (forall (x :: T). Proxy x) :~: (forall (x :: Int). Proxy x)
g = Refl

h :: (forall (x :: S). Proxy x) :~: (forall (x :: Int). Proxy x)
h = f `trans` g
