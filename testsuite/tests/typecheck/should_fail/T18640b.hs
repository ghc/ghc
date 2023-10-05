{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T18640b where

import Data.Kind

data family F1 (k :: Type) :: k

type F3 :: forall (a :: Type) -> forall (b :: Type) -> a
type family F3 a where
  F3 a = F1
