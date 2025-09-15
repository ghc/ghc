{-# LANGUAGE DeriveFoldable, DeriveTraversable, EmptyDataDecls, RoleAnnotations #-}
module T20496 where

data T b = MkT b
  deriving (Functor, Foldable, Traversable)

data Trixie a
  deriving (Functor, Foldable, Traversable)

type role UhOh nominal
data UhOh a
  deriving (Functor, Foldable, Traversable)

type role UhOh2 representational
data UhOh2 a
  deriving (Functor, Foldable, Traversable)
