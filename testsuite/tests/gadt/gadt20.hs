{-# LANGUAGE GADTs, KindSignatures #-}

-- Test for trac #810
-- Should be able to infer bool :: Bool and integer :: Integer, so
-- we should know that they both have Show instances.

module Foo where

import Data.Kind (Type)

data Pair :: (Type -> Type) -> Type where
    Pair :: a b -> b -> Pair a

data Sel :: Type -> Type where
    A :: Sel Bool
    B :: Sel Integer

showSnd :: Pair Sel -> String
showSnd (Pair A bool)    = show bool
showSnd (Pair B integer) = show integer

