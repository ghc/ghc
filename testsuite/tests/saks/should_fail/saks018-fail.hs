{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_018 where

import Data.Kind (Type)

type T :: forall k -> k -> Type
data T k (x :: hk)
