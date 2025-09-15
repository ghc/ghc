{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Foo where

import Data.Kind
import Data.Proxy

type F :: Type -> forall (a :: Type) -> Type
data family F a b
newtype instance F Int b = MkF (Proxy b)
