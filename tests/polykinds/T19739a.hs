{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Bug where

import Data.Kind
import Data.Proxy

type T :: forall (a :: Type) -> Constraint
class T a where
  f :: Proxy a
