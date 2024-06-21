-- Having Map.empty present, does not change the fact that Alternative.empty is not visible
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BugReproduce where

import Control.Applicative (Alternative)
import qualified Data.Map as Map

newtype Foo a = MkFoo [a] deriving (Functor, Applicative)

instance Alternative Foo where
  empty = undefined
