-- Multiple other empty (Map.empty, Data.empty), but the issue still Alternative.empty not visible.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BugReproduce where

import Control.Applicative (Alternative)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Foo a = MkFoo [a] deriving (Functor, Applicative)

instance Alternative Foo where
  empty = undefined
