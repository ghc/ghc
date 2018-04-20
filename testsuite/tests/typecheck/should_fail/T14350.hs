{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T14350 where

import Data.Kind

data Proxy a = Proxy
data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

class SingKind k where
  type Demote k :: Type
  fromSing :: Sing (a :: k) -> Demote k
  toSing   :: Demote k -> SomeSing k

data instance Sing (x :: Proxy k) where
  SProxy :: Sing 'Proxy

instance SingKind (Proxy k) where
  type Demote (Proxy k) = Proxy k
  fromSing SProxy = Proxy
  toSing Proxy = SomeSing SProxy

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

newtype instance Sing (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }

instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
  type Demote (k1 ~> k2) = Demote k1 -> Demote k2
  fromSing sFun x = case toSing x of SomeSing y -> fromSing (applySing sFun y)
  toSing = undefined

dcomp :: forall (a :: Type)
                (b :: a ~> Type)
                (c :: forall (x :: a). Proxy x ~> b @@ x ~> Type)
                (f :: forall (x :: a) (y :: b @@ x). Proxy x ~> Proxy y ~> c @@ ('Proxy :: Proxy x) @@ y)
                (g :: forall (x :: a). Proxy x ~> b @@ x)
                (x :: a).
         Sing f
      -> Sing g
      -> Sing x
      -> c @@ ('Proxy :: Proxy x) @@ (g @@ ('Proxy :: Proxy x))
dcomp f g x = applySing f Proxy Proxy
