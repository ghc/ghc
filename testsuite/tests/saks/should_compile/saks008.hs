{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds #-}

module SAKS_008 where

import Data.Proxy (Proxy)
import Data.Kind (Type)

-- Test inferred type variables.
-- T :: forall {k} (a :: k). Proxy a -> Type
type T :: Proxy a -> Type
data T x = MkT
