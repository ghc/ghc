{-# LANGUAGE DeepSubsumption, RankNTypes, PolyKinds, DataKinds, GADTs, TypeOperators, ScopedTypeVariables #-}
module T27639 where

import GHC.Exts
import Data.Type.Equality

h :: forall r (a :: TYPE r) (b :: TYPE r).
     (r :~: LiftedRep) -> (a :~: b) -> ()
h Refl Refl =
  let
    v :: a -> (Int -> Int) -> Int
    v = undefined
    u :: b -> (forall c. c -> c) -> Int
    u = v
  in ()
