{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module TLKS_015 where

import Data.Kind (Type)

type T :: forall k -> k -> Type
data T (k :: Type) (a :: k)
