{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- A stripped-down version of singletons
module T17139a where

import Data.Kind (Type)
import Data.Proxy (Proxy)

data family Sing :: k -> Type
class SingI a where
  sing :: Sing a
class SingKind k where
  type Demote k = (r :: Type) | r -> k
  toSing   :: Demote k -> Proxy k
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
newtype instance Sing (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (Apply f t) }
instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
  type Demote (k1 ~> k2) = Demote k1 -> Demote k2
  toSing = undefined
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing
