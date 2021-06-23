{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Definitions where

-- base
import Data.Kind
  ( Type, Constraint )
import Numeric.Natural
  ( Natural )

--------------------------------------------------------------------------------

type Nullary :: Constraint
class Nullary where { }

type MyClass :: Type -> Constraint
class MyClass a where
  methC :: a

type MyTyFam :: Type -> Type -> Type
type family MyTyFam a b where

data Nat = Zero | Succ Nat

type Add :: Nat -> Nat -> Nat
type family Add a b where
  Add   Zero     b = b
  Add ( Succ a ) b = Succ ( Add a b )
