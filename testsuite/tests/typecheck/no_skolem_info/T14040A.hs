{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import Data.Kind
import Data.Proxy

newtype S (f :: k1 -> k2)
  = MkS (forall t. Proxy t -> Proxy (f t))

foo :: forall (a :: Type)
              (f :: forall (x :: a). Proxy x -> Type).
       S f -> ()
foo (MkS (sF :: _)) = ()
