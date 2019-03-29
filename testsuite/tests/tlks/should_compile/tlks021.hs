{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module TLKS_021 where

import Data.Kind (Type)

type T :: forall k -> forall (xx :: k) -> Type
data T k (x :: hk)
