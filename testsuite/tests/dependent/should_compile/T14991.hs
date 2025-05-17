{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T14991 where

import Data.Kind

type family Promote (k :: Type) :: Type
type family PromoteX (a :: k) :: Promote k

type family Demote (k :: Type) :: Type
type family DemoteX (a :: k) :: Demote k

-----
-- Type
-----

type instance Demote Type = Type
type instance Promote Type = Type

type instance DemoteX (a :: Type) = Demote a
type instance PromoteX (a :: Type) = Promote a

-----
-- Arrows
-----

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type instance Demote  (a ~> b) = DemoteX  a -> DemoteX  b
type instance Promote ((a :: Type) -> (b :: Type)) = PromoteX a ~> PromoteX b
