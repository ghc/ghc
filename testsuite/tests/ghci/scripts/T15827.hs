{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T15827 where

import Data.Kind
import Data.Proxy

type family F1 (a :: k)
type instance forall k (a :: k). F1 a = Proxy a

type family F2 (a :: k) :: Type where
  forall k (a :: k). F2 a = Proxy a

data family D (a :: k)
data instance forall k (a :: k). D a = MkD (Proxy a)
