{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T16225 where

import Data.Kind

data family Sing :: k -> Type
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: a ~> b) (x :: a) :: b

data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)
type instance Apply (TyCon1 f) x = f x

data SomeApply :: (k ~> Type) -> Type where
  SomeApply :: Apply f a -> SomeApply f

f :: SomeApply (TyCon1 Sing :: k ~> Type)
  -> SomeApply (TyCon1 Sing :: k ~> Type)
f (SomeApply s)
 = SomeApply s
