{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

-- Based on singleton-gadts
module T12088sg3 where

import Data.Kind (Type)

type family Promote (k :: Type) :: Type
type family PromoteX (a :: k) :: Promote k

type instance Promote Type = Type
type instance PromoteX (a :: Type) = Promote a

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

data (~>@#@$) :: Type ~> Type ~> Type
type instance Apply (~>@#@$) a = (~>@#@$$) a
data (~>@#@$$) (a :: Type) :: Type ~> Type
type instance Apply ((~>@#@$$) a) b = a ~> b

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)
data TyCon2 :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3)
type instance Apply (TyCon1 f) x = f x
type instance Apply (TyCon2 f) x = TyCon1 (f x)

type instance Promote (a -> b) = PromoteX a ~> PromoteX b

type Arr :: (Type ~> Type ~> Type) -> (Type -> Type -> Type)
data Arr p a b = MkArr (Apply (Apply p a) b)

type PArr = Arr (~>@#@$)

type instance Promote (Arr (TyCon2 (->)) a b) = PArr (PromoteX a) (PromoteX b)
-- $(return [])
type instance PromoteX (MkArr x :: Arr (TyCon2 (->)) a b) = MkArr (PromoteX x)
