{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_Fail009 where

import Data.Kind (Type)

type T :: forall j -> j -> Type
data T (k :: Type -> Type) (a :: k)
