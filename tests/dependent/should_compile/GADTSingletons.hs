{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- "Singletons for GADTs" by Ryan Scott, Richard Eisenberg.
--
-- This unminimized test case happens to be a good stress test for the
-- dependency analysis and the subsequent retrying logic.

module GADTSingletons where

import Data.Kind
import Data.Type.Equality
import Unsafe.Coerce

type family Sing :: k -> Type

data SomeSing (k :: Type) where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: forall k r
              . SingKind k
             => Demote k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

type family Promote (k :: Type) :: Type
type family PromoteX (a :: k) :: Promote k

type family Demote (k :: Type) :: Type
type family DemoteX (a :: k) :: Demote k

-- hetero is important; otherwise, GHC can't be sure that the kinds
-- respect the Promote/Demote property
type SingKindX (a :: k) = (PromoteX (DemoteX a) ~~ a)

type family SingKindC (a :: k) :: Constraint
type SingKindCX (a :: k) = (SingKindC a, SingKindX a)

class SingKindX k => SingKind k where
  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> SomeSing k

class SingI (a :: k) where
  sing :: Sing a

-----
-- Type
-----

type instance Demote Type = Type
type instance Promote Type = Type

-- NB: There are at least two choices for the implementation of this
-- SingKind instance. To avoid accusations of favoritism, we will avoid
-- picking sides here :)
instance SingKind Type where
  fromSing = error "fromSing Type"
  toSing   = error "toSing Type"

type instance DemoteX (a :: Type) = Demote a
type instance PromoteX (a :: Type) = Promote a

type instance SingKindC (a :: Type) = SingKind a

-----
-- Bool
-----

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
type instance Sing = SBool

type instance Demote Bool = Bool
type instance Promote Bool = Bool

instance SingKind Bool where
  fromSing STrue = True
  fromSing SFalse = False

  toSing True = SomeSing STrue
  toSing False = SomeSing SFalse

-- This could also be expressed as
--
--  type instance DemoteX (b :: Bool) = Bool
--
-- But who knows if singletons will ever be smart enough to figure that out...
type instance DemoteX False = False
type instance DemoteX True  = True
type instance PromoteX False = False
type instance PromoteX True  = True

type instance SingKindC False = ()
type instance SingKindC True  = ()

instance SingI True where
  sing = STrue
instance SingI False where
  sing = SFalse

-----
-- Ordering
-----

data SOrdering :: Ordering -> Type where
  SLT :: SOrdering LT
  SEQ :: SOrdering EQ
  SGT :: SOrdering GT
type instance Sing = SOrdering

type instance Demote Ordering = Ordering
type instance Promote Ordering = Ordering

instance SingKind Ordering where
  fromSing SLT = LT
  fromSing SEQ = EQ
  fromSing SGT = GT

  toSing LT = SomeSing SLT
  toSing EQ = SomeSing SEQ
  toSing GT = SomeSing SGT

type instance DemoteX LT = LT
type instance DemoteX EQ = EQ
type instance DemoteX GT = GT
type instance PromoteX LT = LT
type instance PromoteX EQ = EQ
type instance PromoteX GT = GT

type instance SingKindC LT = ()
type instance SingKindC EQ = ()
type instance SingKindC GT = ()

instance SingI LT where
  sing = SLT

instance SingI EQ where
  sing = SEQ

instance SingI GT where
  sing = SGT

-----
-- List
-----

data SList :: forall a. [a] -> Type where
  SNil  :: SList '[]
  SCons :: Sing x -> Sing xs -> SList (x:xs)
type instance Sing = SList

type instance Demote  [a] = [DemoteX  a]
type instance Promote [a] = [PromoteX a]

instance SingKindCX a => SingKind [a] where
  fromSing SNil         = []
  fromSing (SCons x xs) = fromSing x : fromSing xs
  toSing []     = SomeSing SNil
  toSing (x:xs) = withSomeSing x  $ \x'  ->
                  withSomeSing xs $ \xs' ->
                  SomeSing (SCons x' xs')

type instance DemoteX '[]    = '[]
type instance DemoteX (x:xs) = DemoteX x : DemoteX xs

type instance PromoteX '[]    = '[]
type instance PromoteX (x:xs) = PromoteX x : PromoteX xs

type instance SingKindC '[]    = ()
type instance SingKindC (x:xs) = (SingKindC x, SingKindC xs)

instance SingI '[] where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x:xs) where
  sing = SCons sing sing

-----
-- Simple GADT example 1
-----

data Foo (a :: Type) where MkFoo :: Foo Bool

data SFoo :: forall a. Foo a -> Type where
  SMkFoo :: SFoo MkFoo
type instance Sing = SFoo

type instance Demote (Foo a) = Foo (DemoteX a)
type instance Promote (Foo a) = Foo (PromoteX a)

instance SingKindCX a => SingKind (Foo a) where
  fromSing SMkFoo = MkFoo
  toSing MkFoo = SomeSing SMkFoo

type instance DemoteX MkFoo = MkFoo
type instance PromoteX MkFoo = MkFoo

type instance SingKindC MkFoo = ()

instance SingI MkFoo where
  sing = SMkFoo

-----
-- Simple GADT example 2
-----

data Quux (a :: Type) where
  MkQuux1 :: Quux Bool
  MkQuux2 :: Quux Ordering

data SQuux (z :: Quux a)
  = (z ~~ MkQuux1) => SMkQuux1
  | (z ~~ MkQuux2) => SMkQuux2
type instance Sing = SQuux

type instance Demote (Quux a) = Quux (DemoteX a)
type instance Promote (Quux a) = Quux (PromoteX a)

instance SingKindC a => SingKind (Quux a) where
  fromSing SMkQuux1 = MkQuux1
  fromSing SMkQuux2 = MkQuux2

  toSing MkQuux1 = SomeSing SMkQuux1
  toSing MkQuux2 = SomeSing SMkQuux2

type instance DemoteX MkQuux1 = MkQuux1
type instance DemoteX MkQuux2 = MkQuux2
type instance PromoteX MkQuux1 = MkQuux1
type instance PromoteX MkQuux2 = MkQuux2

type instance SingKindC MkQuux1 = ()
type instance SingKindC MkQuux2 = ()

instance SingI MkQuux1 where
  sing = SMkQuux1

instance SingI MkQuux2 where
  sing = SMkQuux2

-----
-- Peano naturals
-----

data N = Z | S N

data SN :: N -> Type where
  SZ :: SN Z
  SS :: Sing n -> SN (S n)
type instance Sing = SN

type instance Demote N = N
type instance Promote N = N

instance SingKind N where
  fromSing SZ = Z
  fromSing (SS n) = S (fromSing n)

  toSing Z = SomeSing SZ
  toSing (S n) = withSomeSing n $ SomeSing . SS

type instance DemoteX Z = Z
type instance DemoteX (S n) = S (DemoteX n)
type instance PromoteX Z = Z
type instance PromoteX (S n) = S (PromoteX n)

type instance SingKindC Z     = ()
type instance SingKindC (S n) = SingKindC n

instance SingI Z where
  sing = SZ

instance SingI n => SingI (S n) where
  sing = SS sing

-----
-- More complicated GADT example 1
----

data Fin :: N -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

data SFin :: forall n. Fin n -> Type where
  SFZ :: SFin FZ
  SFS :: Sing fn -> SFin (FS fn)
type instance Sing = SFin

type instance Demote (Fin n) = Fin (DemoteX n)
type instance Promote (Fin n) = Fin (PromoteX n)

instance SingKindCX n => SingKind (Fin n) where
  fromSing SFZ = FZ
  fromSing (SFS sfn) = FS (fromSing sfn)

  toSing FZ = SomeSing SFZ
  toSing (FS fn) = withSomeSing fn $ SomeSing . SFS

type instance DemoteX FZ = FZ
type instance DemoteX (FS n) = FS (DemoteX n)
type instance PromoteX FZ = FZ
type instance PromoteX (FS n) = FS (PromoteX n)

type instance SingKindC FZ     = ()
type instance SingKindC (FS n) = SingKindC n

instance SingI FZ where
  sing = SFZ

instance SingI fn => SingI (FS fn) where
  sing = SFS sing

-----
-- More complicated GADT example 2
-----

data Vec (n :: N) (a :: Type) where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data SVec :: forall n a. Vec n a -> Type where
  SVNil  :: SVec VNil
  SVCons :: Sing x -> Sing xs -> SVec (VCons x xs)
type instance Sing = SVec

type instance Demote (Vec n a) = Vec (DemoteX n) (DemoteX a)
type instance Promote (Vec n a) = Vec (PromoteX n) (PromoteX a)

instance (SingKindCX n, SingKindCX a) => SingKind (Vec n a) where
  fromSing SVNil = VNil
  fromSing (SVCons x xs) = VCons (fromSing x) (fromSing xs)

  toSing VNil = SomeSing SVNil
  toSing (VCons x xs) = withSomeSing x  $ \x'  ->
                        withSomeSing xs $ \xs' ->
                        SomeSing $ SVCons x' xs'

type instance DemoteX VNil = VNil
type instance DemoteX (VCons x xs) = VCons (DemoteX x) (DemoteX xs)
type instance PromoteX VNil = VNil
type instance PromoteX (VCons x xs) = VCons (PromoteX x) (PromoteX xs)

type instance SingKindC VNil         = ()
type instance SingKindC (VCons x xs) = (SingKindC x, SingKindC xs)

instance SingI VNil where
  sing = SVNil

instance (SingI x, SingI xs) => SingI (VCons x xs) where
  sing = SVCons sing sing

-----
-- PolyKinds
-----

data Prox (a :: k) = P

data SProx :: forall k (a :: k). Prox a -> Type where
  SP :: SProx P
type instance Sing = SProx

type instance Demote (Prox (a :: k)) = Prox (DemoteX a)
type instance Promote (Prox (a :: k)) = Prox (PromoteX a)

instance SingKindCX a => SingKind (Prox (a :: k)) where
  fromSing SP = P
  toSing P = SomeSing SP

type instance DemoteX P = P
type instance PromoteX P = P

type instance SingKindC P = ()

instance SingI P where
  sing = SP

-----
-- (:~~:)
-----

data (%:~~:) :: forall j k (a :: j) (b :: k). a :~~: b -> Type where
  SHRefl :: (%:~~:) HRefl
type instance Sing = (%:~~:)

type instance Demote  (a :~~: b) = DemoteX  a :~~: DemoteX  b
type instance Promote (a :~~: b) = PromoteX a :~~: PromoteX b

instance (SingKindCX a, SingKindCX b) => SingKind (a :~~: b) where
  fromSing SHRefl = HRefl
  toSing HRefl = SomeSing SHRefl

type instance DemoteX  HRefl = HRefl
type instance PromoteX HRefl = HRefl

type instance SingKindC HRefl = ()

instance SingI HRefl where
  sing = SHRefl

-----
-- HList
-----

data HList :: [Type] -> Type where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x:xs)

data SHList :: forall xs. HList xs -> Type where
  SHNil  :: SHList HNil
  SHCons :: Sing x -> Sing xs -> SHList (HCons x xs)
type instance Sing = SHList

type instance Demote  (HList xs) = HList (DemoteX xs)
type instance Promote (HList xs) = HList (PromoteX xs)

instance SingKindCX xs => SingKind (HList xs) where
  fromSing SHNil = HNil
  fromSing (SHCons x xs) = HCons (fromSing x) (fromSing xs)

  toSing HNil         = SomeSing SHNil
  toSing (HCons x xs) = withSomeSing x  $ \x'  ->
                        withSomeSing xs $ \xs' ->
                        SomeSing (SHCons x' xs')

type instance DemoteX HNil = HNil
type instance DemoteX (HCons x xs) = HCons (DemoteX x) (DemoteX xs)

type instance PromoteX HNil = HNil
type instance PromoteX (HCons x xs) = HCons (PromoteX x) (PromoteX xs)

type instance SingKindC HNil         = ()
type instance SingKindC (HCons x xs) = (SingKindC x, SingKindC xs)

instance SingI HNil where
  sing = SHNil

instance (SingI x, SingI xs) => SingI (HCons x xs) where
  sing = SHCons sing sing

-----
-- Arrows
-----

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

data (~>@#@$) :: Type ~> Type ~> Type
type instance Apply (~>@#@$) a = (~>@#@$$) a
data (~>@#@$$) (a :: Type) :: Type ~> Type
type instance Apply ((~>@#@$$) a) b = a ~> b

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

type SingFunction1 f = forall t. Sing t -> Sing (f @@ t)
singFun1 :: forall f. SingFunction1 f -> Sing f
singFun1 f = SLambda f

data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)
data TyCon2 :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3)
type instance Apply (TyCon1 f) x = f x
type instance Apply (TyCon2 f) x = TyCon1 (f x)

type instance Demote  (a ~> b) = DemoteX  a -> DemoteX  b
type instance Promote (a -> b) = PromoteX a ~> PromoteX b

newtype SLambda (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }
type instance Sing = SLambda

instance (SingKindC k1, SingKindC k2) => SingKind (k1 ~> k2) where
  fromSing sFun x = withSomeSing x $ fromSing . applySing sFun
  toSing f = SomeSing slam
    where
      slam :: forall (f :: k1 ~> k2). Sing f
      slam = singFun1 @f lam
        where
          lam :: forall (t :: k1). Sing t -> Sing (f @@ t)
          lam x = withSomeSing (f (fromSing x)) (\(r :: Sing res) -> unsafeCoerce r)

-----
-- Example instances for type constructors
-----

type instance DemoteX (TyCon1 (Either a)) = Either (DemoteX a)
type instance PromoteX (Either a) = TyCon1 (Either (PromoteX a))
type instance SingKindC (TyCon1 (Either a)) = SingKind a

type instance DemoteX (TyCon2 Either) = Either
type instance PromoteX Either = TyCon2 Either
type instance SingKindC Either = ()

-----
-- Arrow example
-----

newtype Arr' (p :: Type ~> Type ~> Type) (a :: Type) (b :: Type)
  = MkArr (p @@ a @@ b)

type Arr  = Arr' (TyCon2 (->))
type PArr = Arr' (~>@#@$)

type instance Demote  (PArr a b) = Arr  (DemoteX  a) (DemoteX  b)
type instance Promote (Arr  a b) = PArr (PromoteX a) (PromoteX b)

data SArr :: forall a b. PArr a b -> Type where
  SMkArr :: Sing x -> SArr (MkArr x)
type instance Sing = SArr

instance (SingKindCX a, SingKindCX b) => SingKind (PArr a b) where
  fromSing (SMkArr x) = MkArr (fromSing x)
  toSing (MkArr x) = withSomeSing x $ SomeSing . SMkArr

type instance DemoteX  (MkArr x :: PArr a b) = MkArr (DemoteX  x)
type instance PromoteX (MkArr x :: Arr  a b) = MkArr (PromoteX x)

type instance SingKindC (MkArr x) = SingKindC x

instance SingI x => SingI (MkArr x :: PArr a b) where
  sing = SMkArr sing
