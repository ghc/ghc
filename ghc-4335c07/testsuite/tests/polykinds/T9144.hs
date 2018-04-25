{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, GADTs, RankNTypes #-}

module T9144 where

import Data.Proxy
import GHC.TypeLits

data family Sing (a :: k)

data SomeSing :: KProxy k -> * where
  SomeSing :: forall (a :: k). Sing a -> SomeSing ('KProxy :: KProxy k)

class kproxy ~ 'KProxy => SingKind (kproxy :: KProxy k) where
  fromSing :: forall (a :: k). Sing a -> DemoteRep ('KProxy :: KProxy k)
  toSing :: DemoteRep ('KProxy :: KProxy k) -> SomeSing ('KProxy :: KProxy k)

type family DemoteRep (kproxy :: KProxy k) :: *

data Foo = Bar Nat
data FooTerm = BarTerm Integer

data instance Sing (x :: Foo) where
  SBar :: Sing n -> Sing (Bar n)

type instance DemoteRep ('KProxy :: KProxy Nat) = Integer
type instance DemoteRep ('KProxy :: KProxy Foo) = FooTerm

instance SingKind ('KProxy :: KProxy Nat) where
  fromSing = undefined
  toSing = undefined

instance SingKind ('KProxy :: KProxy Foo) where
  fromSing (SBar n) = BarTerm (fromSing n)
  toSing n = case toSing n of SomeSing n' -> SomeSing (SBar n')
