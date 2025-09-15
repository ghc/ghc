{-# Language PolyKinds, DataKinds, KindSignatures, GADTs, ConstraintKinds #-}
module T14845_fail1 where

import Data.Kind

data Struct :: (k -> Constraint) -> (k -> Type) where
  S :: cls a => Struct cls a

data Foo a where
  F :: Foo (S::Struct Eq a)
