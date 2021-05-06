{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Bug where

import Data.Kind
import Data.Proxy

type T :: forall (a :: Type) -> Type
newtype T a = Mk (Proxy a)
