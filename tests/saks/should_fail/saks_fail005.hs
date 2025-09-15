{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, PolyKinds #-}

module SAKS_Fail005 where

import Data.Kind (Type)
import Data.Proxy (Proxy)

-- GADT constructors do not run under bindTyClTyVars,
-- and thus have no access to scoped type variables.
type G :: forall k. k -> Type
data G a where
  MkG :: forall a. Proxy (a :: k) -> G a
