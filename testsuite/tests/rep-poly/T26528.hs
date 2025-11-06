{-# LANGUAGE GHC2024, TypeFamilies #-}

module T26528 where

import Data.Kind
import GHC.Exts

type F :: Type -> RuntimeRep
type family F a where
  F Int = LiftedRep

g :: forall (r::RuntimeRep).
     (forall (a :: TYPE r). a -> forall b. b -> b) -> Int
g _ = 3
{-# NOINLINE g #-}

foo = g @(F Int) (\x y -> y)
