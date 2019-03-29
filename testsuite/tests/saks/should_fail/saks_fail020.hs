{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}

module SAKS_Fail020 where

import Data.Kind
import Data.Proxy

type Foo2 :: () -> forall (k :: Type) -> Proxy (a :: k)
type family Foo2 d k where {}

