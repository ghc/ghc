{-# LANGUAGE
    QuantifiedConstraints
  , StandaloneKindSignatures
  , TypeOperators
  , GADTs
  , ConstraintKinds
  , RankNTypes
  , UndecidableInstances
  , ImpredicativeTypes
#-}
module Typelevel.Constraint.Repro where

import Data.Kind (Constraint, Type)

type Dict :: Constraint -> Type
data Dict c
  where
  Dict :: c => Dict c

type (⇒) :: Constraint -> Constraint -> Constraint
type c ⇒ d = c => d
infixr 0 ⇒

type (\/) :: Constraint -> Constraint -> Constraint
type a \/ b = (forall r. (a ⇒ r, b ⇒ r) ⇒ r)
infixr 5 \/

dict :: Dict ((x \/ y) \/ z ⇒ x \/ y \/ z)
dict = Dict
