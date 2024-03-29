{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_021 where

import Data.Kind (Type)

type T :: forall k -> forall (xx :: k) -> Type
data T k (x :: hk)
