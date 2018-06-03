{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module T13777 where

import Data.Kind
import Data.Proxy

data S :: forall k. Proxy k -> Type where
  MkS :: S ('Proxy :: Proxy Maybe)

data T (a :: b) :: forall c (d :: Type) e.
                   (forall f. Proxy f) -> Proxy c -> Proxy d -> Proxy e
                -> Type where
