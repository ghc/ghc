{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module TLKS_018 where

import Data.Kind (Type)

type T :: forall k -> k -> Type
data T k (x :: hk)
