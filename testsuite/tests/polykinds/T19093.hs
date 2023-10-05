{-# Language RankNTypes, TypeApplications, PolyKinds, DataKinds,
             TypeOperators, StandaloneKindSignatures, TypeFamilies,
             FlexibleInstances, MultiParamTypeClasses #-}

module T19093 where

import Data.Proxy
import Data.Type.Equality
import Data.Kind

type PolyKinded :: Type -> Type
type PolyKinded res = (forall (k :: Type). k -> res)


type  TryUnify :: PolyKinded (PolyKinded Constraint)
-- type TryUnify :: Bool -> forall k. k -> forall k. k -> Constraint
-- type TryUnify :: Bool -> PolyKinded (forall k. k -> Constraint)

class TryUnify a b where
