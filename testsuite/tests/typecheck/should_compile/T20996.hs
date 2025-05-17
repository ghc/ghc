{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language DataKinds #-}
module T20996 (Generic1 (..), Rep1) where
import GHC.Generics hiding (Generic1 (..))
import Data.Kind
import Data.Coerce
import GHC.TypeLits

data family LastPar :: k
data family OtherPar :: k -> k

type Generic1 :: forall {k}. (k -> Type) -> Constraint
class Generic1 (f :: k -> Type) where
  to1 :: forall a. Rep1 f a -> f a
  from1 :: forall a. f a -> Rep1 f a

instance forall k (f :: k -> Type).
  (forall (a :: k). GenericQ f a) => Generic1 (f :: k -> Type) where

  to1 :: forall a. Rep1 f a -> f a
  to1 = toQ

  from1 :: forall a. f a -> Rep1 f a
  from1 = fromQ

type GenericQ :: forall {k}. (k -> Type) -> k -> Constraint
class GenericQ (f :: k -> Type) (a :: k) where
  toQ :: Rep1 f a -> f a
  fromQ :: f a -> Rep1 f a

instance forall k (f :: k -> Type) (a :: k) rfa r1f.
  (Generic1' rfa r1f a, rfa ~ Rep (f a), r1f ~ Rep1 f, Generic (f a)) => GenericQ f a where
  toQ :: Rep1 f a -> f a
  toQ = to . from1' @rfa @r1f @a

  fromQ :: f a -> Rep1 f a
  fromQ = to1' @rfa @r1f @a . from

type Generic1' :: forall {k}. (Type -> Type) -> (k -> Type) -> k -> Constraint
class Generic1' (rep :: Type -> Type) (rep1 :: k -> Type) (a :: k) where
  to1' :: rep p -> rep1 a
  from1' :: rep1 a -> rep p

instance Generic1' f f' a => Generic1' (M1 i c f) (M1 i c f') a where
  to1' (M1 x) = M1 (to1' @f @f' @a x)
  from1' (M1 x) = M1 (from1' @f @f' @a x)

instance (Generic1' f f' a, Generic1' g g' a) => Generic1' (f :*: g) (f' :*: g') a where
  to1' (x :*: y) = to1' @f @f' @a x :*: to1' @g @g' @a y
  from1' (x :*: y) = from1' @f @f' @a x :*: from1' @g @g' @a y

instance (Generic1' f f' a, Generic1' g g' a) => Generic1' (f :+: g) (f' :+: g') a where
  to1' (L1 x) = L1 (to1' @f @f' @a x)
  to1' (R1 y) = R1 (to1' @g @g' @a y)
  from1' (L1 x) = L1 (from1' @f @f' @a x)
  from1' (R1 y) = R1 (from1' @g @g' @a y)

instance Generic1' U1 U1 a where
  to1' U1 = U1
  from1' U1 = U1

instance Generic1' V1 V1 a where
  to1' x = case x of
  from1' x = case x of

-- This instance is pure gold.
instance Coercible c (r a) => Generic1' (Rec0 c) r a where
  to1' (K1 x) = coerce @c @(r a) x
  from1' x = K1 (coerce @(r a) @c x)

type Rep1 (f :: k -> Type) = MakeRep1' @k (Rep ((MarkOtherPars f) (LastPar :: k)))

type MakeRep1' :: forall k. (Type -> Type) -> k -> Type
type family MakeRep1' (rep :: Type -> Type) :: k -> Type where
  MakeRep1' (M1 i c f) = M1 i c (MakeRep1' f)
  MakeRep1' (x :+: y) = MakeRep1' x :+: MakeRep1' y
  MakeRep1' (x :*: y) = MakeRep1' x :*: MakeRep1' y
  MakeRep1' U1 = U1
  MakeRep1' V1 = V1
  MakeRep1' (Rec0 c) = MakeRep1Field (Rec0 (Unmark c)) Par1 c

type MarkOtherPars :: forall k. k -> k
type family MarkOtherPars (f :: k) :: k where
  MarkOtherPars ((f :: j -> k) (a :: j)) = MarkOtherPars f (OtherPar a)
  MarkOtherPars f = f

type Unmark :: forall k. k -> k
type family Unmark (f :: k) :: k where
  Unmark LastPar = TypeError ('Text "Cannot create Generic1 instance: the last parameter appears in an invalid location.")
  Unmark (OtherPar a) = a
  Unmark ((f :: j -> k) (a :: j)) = Unmark f (Unmark a)
  Unmark a = a

type MakeRep1Field :: forall j k. (k -> Type) -> (j -> Type) -> j -> k -> Type
type family MakeRep1Field fk acc c where
  MakeRep1Field fk (_ :: b -> Type) (OtherPar _) = fk
  MakeRep1Field fk (acc :: b -> Type) ((f :: a -> b) (x :: a)) = MakeRep1Field fk (acc :.: Unmark f) x
  MakeRep1Field @_ @k fk (acc :: k -> Type) (LastPar :: k) = acc
  MakeRep1Field fk _ _ = fk
