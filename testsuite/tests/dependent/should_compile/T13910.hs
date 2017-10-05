{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T13910 where

import Data.Kind
import Data.Type.Equality

data family Sing (a :: k)

class SingKind k where
  type Demote k = (r :: *) | r -> k
  fromSing :: Sing (a :: k) -> Demote k
  toSing   :: Demote k -> SomeSing k

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: forall k r
              . SingKind k
             => Demote k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

data TyFun :: * -> * -> *
type a ~> b = TyFun a b -> *
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

data FunArrow = (:->) | (:~>)

class FunType (arr :: FunArrow) where
  type Fun (k1 :: Type) arr (k2 :: Type) :: Type

class FunType arr => AppType (arr :: FunArrow) where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2

type FunApp arr = (FunType arr, AppType arr)

instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2

$(return []) -- This is only necessary for GHC 8.0 -- GHC 8.2 is smarter

instance AppType (:->) where
  type App k1 (:->) k2 (f :: k1 -> k2) x = f x

instance FunType (:~>) where
  type Fun k1 (:~>) k2 = k1 ~> k2

$(return [])

instance AppType (:~>) where
  type App k1 (:~>) k2 (f :: k1 ~> k2) x = f @@ x

infixr 0 -?>
type (-?>) (k1 :: Type) (k2 :: Type) (arr :: FunArrow) = Fun k1 arr k2

data instance Sing (z :: a :~: b) where
  SRefl :: Sing Refl

instance SingKind (a :~: b) where
  type Demote (a :~: b) = a :~: b
  fromSing SRefl = Refl
  toSing Refl = SomeSing SRefl

(~>:~:) :: forall (k :: Type) (a :: k) (b :: k) (r :: a :~: b) (p :: forall (y :: k). a :~: y ~> Type).
           Sing r
        -> p @@ Refl
        -> p @@ r
(~>:~:) SRefl pRefl = pRefl

type WhyReplacePoly (arr :: FunArrow) (from :: t) (p :: (t -?> Type) arr)
                    (y :: t) (e :: from :~: y) = App t arr Type p y
data WhyReplacePolySym (arr :: FunArrow) (from :: t) (p :: (t -?> Type) arr)
  :: forall (y :: t). from :~: y ~> Type
type instance Apply (WhyReplacePolySym arr from p :: from :~: y ~> Type) x
  = WhyReplacePoly arr from p y x

replace :: forall (t :: Type) (from :: t) (to :: t) (p :: t -> Type).
           p from
        -> from :~: to
        -> p to
replace = replacePoly @(:->)

replaceTyFun :: forall (t :: Type) (from :: t) (to :: t) (p :: t ~> Type).
                p @@ from
             -> from :~: to
             -> p @@ to
replaceTyFun = replacePoly @(:~>) @_ @_ @_ @p

replacePoly :: forall (arr :: FunArrow) (t :: Type) (from :: t) (to :: t)
                      (p :: (t -?> Type) arr).
               FunApp arr
            => App t arr Type p from
            -> from :~: to
            -> App t arr Type p to
replacePoly from eq =
  withSomeSing eq $ \(singEq :: Sing r) ->
    (~>:~:) @t @from @to @r @(WhyReplacePolySym arr from p) singEq from

type WhyLeibnizPoly (arr :: FunArrow) (f :: (t -?> Type) arr) (a :: t) (z :: t)
  = App t arr Type f a -> App t arr Type f z
data WhyLeibnizPolySym (arr :: FunArrow) (f :: (t -?> Type) arr) (a :: t)
  :: t ~> Type
type instance Apply (WhyLeibnizPolySym arr f a) z = WhyLeibnizPoly arr f a z

leibnizPoly :: forall (arr :: FunArrow) (t :: Type) (f :: (t -?> Type) arr)
                      (a :: t) (b :: t).
               FunApp arr
            => a :~: b
            -> App t arr Type f a
            -> App t arr Type f b
leibnizPoly = replaceTyFun @t @a @b @(WhyLeibnizPolySym arr f a) id

leibniz :: forall (t :: Type) (f :: t -> Type) (a :: t) (b :: t).
           a :~: b
        -> f a
        -> f b
leibniz = replaceTyFun @t @a @b @(WhyLeibnizPolySym (:->) f a) id
-- The line above is what you get if you inline the definition of leibnizPoly.
-- It causes a panic, however.
--
-- An equivalent implementation is commented out below, which does *not*
-- cause GHC to panic.
--
-- leibniz = leibnizPoly @(:->)

leibnizTyFun :: forall (t :: Type) (f :: t ~> Type) (a :: t) (b :: t).
                a :~: b
             -> f @@ a
             -> f @@ b
leibnizTyFun = leibnizPoly @(:~>) @_ @f
