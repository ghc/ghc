{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module SAKS_035 where

import Data.Proxy
import Data.Kind

type C :: Proxy i -> Constraint
class C (a :: Proxy z) where
  -- F :: k -> Type
  type F z
