{-# LANGUAGE RoleAnnotations #-}

module T23731 where

import Data.Monoid (Sum(..))
import Data.Coerce (coerce)

type role Foo nominal
data Foo a = Foo a
  deriving (Ord, Eq)

foo :: Foo Int
foo = coerce (Foo (10 :: Sum Int))
