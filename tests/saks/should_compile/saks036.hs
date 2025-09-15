{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, PolyKinds, RankNTypes, DataKinds #-}

module SAKS_036 where

import Data.Kind

type C :: forall (k :: Type). k -> Constraint
class C (a :: (j :: Star)) where
  type F j

type family Star where Star = Type
