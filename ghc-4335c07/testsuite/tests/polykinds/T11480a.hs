{-# language KindSignatures, PolyKinds, TypeFamilies,
  NoImplicitPrelude, FlexibleContexts,
  MultiParamTypeClasses, GADTs,
  ConstraintKinds, FlexibleInstances, UndecidableInstances,
  FunctionalDependencies, UndecidableSuperClasses #-}

module T11480a where

import GHC.Types (Constraint)
import qualified Prelude

data Nat (c :: i -> i -> *) (d :: j -> j -> *) (f :: i -> j) (g :: i -> j)

class Functor p (Nat p (->)) p => Category (p :: i -> i -> *)

class (Category dom, Category cod)
   => Functor (dom :: i -> i -> *) (cod :: j -> j -> *) (f :: i -> j)
    | f -> dom cod

instance (Category c, Category d) => Category (Nat c d)
instance (Category c, Category d) => Functor (Nat c d) (Nat (Nat c d) (->)) (Nat c d)
instance (Category c, Category d) => Functor (Nat c d) (->) (Nat c d f)

instance Category (->)
instance Functor (->) (->) ((->) e)
instance Functor (->) (Nat (->) (->)) (->)
