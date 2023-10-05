{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}

module SAKS_Fail022 where

import Data.Kind
import Data.Proxy

type C :: (x,y) -> Constraint
class C (a :: k) where
  type F k
