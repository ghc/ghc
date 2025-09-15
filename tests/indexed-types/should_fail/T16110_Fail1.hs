{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T16110_Fail1 where

import Data.Kind

class C (a :: Type) where
  type T1 a b
  type forall. T1 a b = Either a b

  type T2 a b
  type forall dup dup dup a b. T2 a b = Either a b

  type T3 a b
  type forall (a :: a) b. T3 a b = Either a b

  type T4 a b
  type forall (a :: k) k b. T4 a b = Either a b
