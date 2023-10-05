{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, GADTs #-}

module T15743e where

import Data.Proxy
import Data.Kind

-- NO CUSK.
data T k (a :: k) (b :: Proxy k2) f c :: forall k3. Proxy k3 -> forall (k4 :: k5). Proxy k4 -> Type where
  MkT :: f c -> T k a b f c d e

-- Want:
-- T :: forall {k3} {k7} {k6} (k2 :: k3) (k5 :: Type).
--      forall k -> k -> Proxy k2 -> (k7 -> Type) -> k4 ->
--      forall (k3 :: k6). Proxy k3 -> forall (k4 :: k5). Proxy k4 -> Type
--
--

-- CUSK
data T2 (k :: Type) (a :: k) (b :: Proxy k2) (f :: k7 -> Type) (c :: k7) :: forall k3. Proxy k3 -> forall k5 (k4 :: k5). Proxy k4 -> Type where
  MkT2 :: f c -> T2 k a b f c d e
