{-# Language PolyKinds, TypeFamilies #-}

module T12041 where

import Data.Kind

class Category (p :: i -> i -> Type) where
  type Ob p :: i -> Constraint

data I a b
instance Category I where
  type Ob @Type I = (~) Int
