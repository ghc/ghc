{-# LANGUAGE PolyKinds, GADTs, TypeApplications, TypeInType, DataKinds,
    RankNTypes, ConstraintKinds, TypeFamilies #-}

module T12045a where

import Data.Kind
import Data.Typeable

data T (f :: k -> Type) a = MkT (f a)

newtype TType f a= MkTType (T @Type f a)

t1 :: TType Maybe Bool
t1 = MkTType (MkT (Just True))

t2 :: TType Maybe a
t2 = MkTType (MkT Nothing)

data Nat = O | S Nat

data T1 :: forall k1 k2. k1 -> k2 -> Type where
  MkT1 :: T1 a b

x :: T1 @_ @Nat False n
x = MkT1

-- test from trac 12045
type Cat k = k -> k -> Type

data FreeCat :: Cat k -> Cat k where
  Nil  :: FreeCat f a a
  Cons :: f a b -> FreeCat f b c -> FreeCat f a c

liftCat :: f a b -> FreeCat f a b
liftCat x = Cons x Nil

data Node = Unit | N

data NatGraph :: Cat Node where
  One  :: NatGraph Unit N
  Succ :: NatGraph N    N

one :: (FreeCat @Node NatGraph) Unit N
one = liftCat One

type Typeable1 = Typeable @(Type -> Type)
type Typeable2 = Typeable @(Type -> Type -> Type)
type Typeable3 = Typeable @(Cat Bool)

type family F a where
  F Type = Type -> Type
  F (Type -> Type) = Type
  F other = other

data T2 :: F k -> Type

foo :: T2 @Type Maybe -> T2 @(Type -> Type) Int -> Type
foo a b = undefined

data family D (a :: k)
data instance D @Type a = DBool
data instance D @(Type -> Type) b = DChar

class C a where
  tc :: (D a) -> Int

instance C Int where
  tc DBool = 5

instance C Bool where
  tc DBool = 6

instance C Maybe where
  tc DChar = 7

-- Tests from D5229
data P a = MkP
type MkPTrue = MkP @Bool

type BoolEmpty = '[] @Bool

type family F1 (a :: k) :: Type
type G2 (a :: Bool) = F1 @Bool a
