{-# LANGUAGE DataKinds, TypeFamilies, TypeFamilyDependencies #-}
module T23135 where

import Data.Kind
import GHC.TypeLits

data Tuple2 x y = MkTuple2 x y

type TupleArgKind :: Nat -> Type
type family TupleArgKind n = r | r -> n where
  TupleArgKind 2 = Tuple2 Type Type

type Tuple :: forall (n :: Nat). TupleArgKind n -> Type
type family Tuple ts = r | r -> ts where
  Tuple (MkTuple2 a b) = Tuple2 a b

-- works
f :: Tuple (MkTuple2 a b) -> ()
f (MkTuple2 _ _) = ()

-- works (failed before the fix)
g :: Tuple (MkTuple2 a b) -> ()
g x = ()

-- works
h :: Tuple @2 (MkTuple2 a b) -> ()
h x = ()

