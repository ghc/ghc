{-# LANGUAGE TypeInType, RankNTypes, TypeFamilies #-}

module T15740a where

import Data.Kind
import Data.Proxy

type family F2 :: forall k. k -> Type

-- This should succeed
type instance F2 = Proxy

