{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module SAKS_034 where

import Data.Kind

type C :: j -> Constraint
class C (a :: k) where
  -- T :: forall j -> j -> Type
  type T k (b :: k) :: Type
