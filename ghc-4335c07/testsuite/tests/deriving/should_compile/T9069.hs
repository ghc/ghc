{-# LANGUAGE DeriveTraversable #-}

module T9069 where

import Data.Foldable
import Data.Traversable

data Trivial a = Trivial a
   deriving (Functor,Foldable,Traversable)