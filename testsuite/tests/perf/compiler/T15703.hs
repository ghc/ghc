{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T15703 where

import Data.Kind
import Data.Type.Equality
import GHC.Generics

data family Sing :: forall k. k -> Type
data instance Sing :: forall a b. (a, b) -> Type where
  STuple2 :: Sing x -> Sing y -> Sing '(x, y)

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type f @@ x = f `Apply` x
infixl 9 @@

newtype instance Sing (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }

type SingFunction1 f = forall t. Sing t -> Sing (f @@ t)
singFun1 :: forall f. SingFunction1 f -> Sing f
singFun1 f = SLambda f

type SingFunction2 f = forall t. Sing t -> SingFunction1 (f @@ t)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> singFun1 (f x))

type SingFunction3 f = forall t. Sing t -> SingFunction2 (f @@ t)
singFun3 :: forall f. SingFunction3 f -> Sing f
singFun3 f = SLambda (\x -> singFun2 (f x))

(@@) :: forall k1 k2 (f :: k1 ~> k2) (t :: k1). Sing f -> Sing t -> Sing (f @@ t)
x @@ y = x `applySing` y

type family ((f :: b ~> c) :. (g :: a ~> b)) (x :: a) :: c where
  (f :. g) x = f @@ (g @@ x)

data (.@#@$) :: forall b c a. (b ~> c) ~> (a ~> b) ~> (a ~> c)
type instance Apply (.@#@$) f = (.@#@$$) f
data (.@#@$$) :: forall b c a. (b ~> c) -> (a ~> b) ~> (a ~> c)
type instance Apply ((.@#@$$) f) g = f .@#@$$$ g
data (.@#@$$$) :: forall b c a. (b ~> c) -> (a ~> b) -> (a ~> c)
type instance Apply (f .@#@$$$ g) x = (f :. g) x

(%.) :: forall b c a (f :: b ~> c) (g :: a ~> b) (x :: a).
        Sing f -> Sing g -> Sing x -> Sing ((f :. g) x)
(f %. g) x = f @@ (g @@ x)

type family Id (x :: a) :: a where
  Id x = x

data IdSym0 :: forall a. a ~> a
type instance Apply IdSym0 x = Id x

sId :: forall a (x :: a). Sing x -> Sing (Id x)
sId x = x

data instance Sing :: forall k c (p :: k). K1 i c p -> Type where
  SK1 :: Sing x -> Sing ('K1 x)

data instance Sing :: forall k i (c :: Meta) (f :: k -> Type) (p :: k).
                      M1 i c f p -> Type where
  SM1 :: Sing x -> Sing ('M1 x)
data M1Sym0 :: forall k i (c :: Meta) (f :: k -> Type) (p :: k).
               f p ~> M1 i c f p
type instance Apply M1Sym0 x = 'M1 x

data instance Sing :: forall k (f :: k -> Type) (g :: k -> Type) (p :: k).
                      (f :*: g) p -> Type where
  (:%*:) :: Sing x -> Sing y -> Sing (x ':*: y)

data instance Sing :: forall p. Par1 p -> Type where
  SPar1 :: Sing x -> Sing ('Par1 x)

class PGeneric1 (f :: k -> Type) where
  type From1 (z :: f a)      :: Rep1 f a
  type To1   (z :: Rep1 f a) :: f a

class SGeneric1 (f :: k -> Type) where
  sFrom1 :: forall (a :: k) (z :: f a).      Sing z -> Sing (From1 z)
  sTo1   :: forall (a :: k) (r :: Rep1 f a). Sing r -> Sing (To1 r :: f a)

class (PGeneric1 f, SGeneric1 f) => VGeneric1 (f :: k -> Type) where
  sTof1 :: forall (a :: k) (z :: f a).      Sing z -> To1 (From1 z)        :~: z
  sFot1 :: forall (a :: k) (r :: Rep1 f a). Sing r -> From1 (To1 r :: f a) :~: r

instance PGeneric1 ((,) a) where
  type From1 '(x, y) = 'M1 ('M1 ('M1 ('K1 x) ':*: 'M1 ('Par1 y)))
  type To1   ('M1 ('M1 ('M1 ('K1 x) ':*: 'M1 ('Par1 y)))) = '(x, y)

instance SGeneric1 ((,) a) where
  sFrom1 (STuple2 x y) = SM1 (SM1 (SM1 (SK1 x) :%*: SM1 (SPar1 y)))
  sTo1   (SM1 (SM1 (SM1 (SK1 x) :%*: SM1 (SPar1 y)))) = STuple2 x y

instance VGeneric1 ((,) a) where
  sTof1 STuple2{} = Refl
  sFot1 (SM1 (SM1 (SM1 SK1{} :%*: SM1 SPar1{}))) = Refl

class PSemigroup a where
  type (x :: a) <> (y :: a) :: a

class SSemigroup a where
  (%<>) :: forall (x :: a) (y :: a).
           Sing x -> Sing y -> Sing (x <> y)

class (PSemigroup a, SSemigroup a) => VSemigroup a where
  semigroupAssociative :: forall (x :: a) (y :: a) (z :: a).
                          Sing x -> Sing y -> Sing z
                       -> (x <> (y <> z)) :~: ((x <> y) <> z)

class PSemigroup a => PMonoid a where
  type Mempty :: a

class SSemigroup a => SMonoid a where
  sMempty :: Sing (Mempty :: a)

class (PMonoid a, SMonoid a, VSemigroup a) => VMonoid a where
  monoidLeftIdentity  :: forall (x :: a).
                         Sing x -> (Mempty <> x) :~: x
  monoidRightIdentity :: forall (x :: a).
                         Sing x -> (x <> Mempty) :~: x
