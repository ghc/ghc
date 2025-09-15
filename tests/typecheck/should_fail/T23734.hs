{-# LANGUAGE TypeFamilies, ExplicitForAll, PolyKinds #-}
module T23734 where

import Data.Kind

type family F
type instance forall a. F = ()

type family G where
  forall b. G = ()

class C where
  type family H
  type forall c. H = ()

data family D :: Type
data instance forall (d :: Type). D = MkD
