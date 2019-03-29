{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, PolyKinds #-}

module SAKS_Fail006 where

import Data.Kind (Type)

-- Type family equations do not run under bindTyClTyVars,
-- and thus have no access to scoped type variables.
type F :: forall k. k -> k
type family F a where
  F (Maybe a) = F @k a
  F x = x
