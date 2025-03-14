{-# LANGUAGE TypeFamilies, DataKinds, TemplateHaskell #-}

-- Based on singletons-gadts
module T12088sg2 where

import Data.Kind

type family Demote (k :: Type) :: Type
type family DemoteX (a :: k) :: Demote k

type instance Demote Type = Type
type instance DemoteX (a :: Type) = Demote a

data TyFun :: Type -> Type -> Type

$(return [])

data TyCon1 :: (k1 -> k2) -> TyFun k1 k2 -> Type

type instance Demote  (TyFun a b -> Type) = DemoteX a -> DemoteX b
type instance DemoteX (TyCon1 (Either a)) = Either (DemoteX a)

