{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))

type TyFun :: Type -> Type -> Type
data TyFun a b

type (~>) :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply f x

type Rep1 :: (k -> Type) -> k -> Type
type family Rep1 f

type To1 :: Rep1 f a -> f a
type family To1 z

type Par1 :: Type -> Type
newtype Par1 p = Par1 p

type To1Sym0 :: forall k (f :: k -> Type) (a :: k). Rep1 f a ~> f a
data To1Sym0 z

applyTo1Sym0 :: forall k (f :: k -> Type) (a :: k) (x :: Rep1 f a). Proxy x -> Apply @(Rep1 f a) @(f a) To1Sym0 x :~: To1 x
applyTo1Sym0 _ = undefined

type IdentitySym0 :: a ~> Identity a
data IdentitySym0 z
type instance Apply IdentitySym0 x = 'Identity x

type Aux :: Rep1 Identity a -> Rep1 Identity a -> Identity a
type family Aux x y where
  Aux x ('Par1 wombat) = Apply IdentitySym0 wombat

type instance Rep1 Identity = Par1
type instance To1 @Identity y = Aux y y

lemma :: forall a (z :: a). Apply @(Rep1 Identity a) @(Identity a) To1Sym0 ('Par1 z) :~: 'Identity z
lemma = applyTo1Sym0 @Type @Identity @a (Proxy @('Par1 z))
-- lemma = applyTo1Sym0 @Type @Identity @a @('Par1 z) (Proxy @('Par1 z))
