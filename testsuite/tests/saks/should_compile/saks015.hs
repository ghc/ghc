{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_015 where

import Data.Kind (Type)

type T :: forall k -> k -> Type
data T (k :: Type) (a :: k)
