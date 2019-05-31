{-# language KindSignatures, PolyKinds, TypeFamilies,
  NoImplicitPrelude, FlexibleContexts,
  MultiParamTypeClasses, GADTs,
  ConstraintKinds, FlexibleInstances, UndecidableInstances,
  FunctionalDependencies, UndecidableSuperClasses,
  TopLevelKindSignatures #-}

module T11480a where

import Data.Kind (Type, Constraint)
import qualified Prelude

data Nat (c :: i -> i -> Type) (d :: j -> j -> Type) (f :: i -> j) (g :: i -> j)

type Category :: (i -> i -> Type) -> Constraint
class Functor p (Nat p (->)) p => Category p

class (Category dom, Category cod)
   => Functor (dom :: i -> i -> Type) (cod :: j -> j -> Type) (f :: i -> j)
    | f -> dom cod

instance (Category c, Category d) => Category (Nat c d)
instance (Category c, Category d) => Functor (Nat c d) (Nat (Nat c d) (->)) (Nat c d)
instance (Category c, Category d) => Functor (Nat c d) (->) (Nat c d f)

instance Category (->)
instance Functor (->) (->) ((->) e)
instance Functor (->) (Nat (->) (->)) (->)
