{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}

module TLKS_Fail023 where

import Data.Kind
import Data.Proxy

type C :: Type -> Constraint
class C (a :: k) where
  type F :: k -> k
