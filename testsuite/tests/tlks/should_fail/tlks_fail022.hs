{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}

module TLKS_Fail022 where

import Data.Kind
import Data.Proxy

type C :: (x,y) -> Constraint
class C (a :: k) where
  type F k
