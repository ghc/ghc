{-# LANGUAGE EmptyDataDecls, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module T4220 where

import Data.Foldable
import Data.Traversable

data Void a deriving (Functor, Foldable, Traversable)
