-- A program with empty (Alternative.empty, Map.empty, Set.empty) builds
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BugReproduce where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Foo a = MkFoo [a] deriving (Functor, Applicative)

instance Alternative Foo where
  empty = undefined
  p <|> q = undefined
