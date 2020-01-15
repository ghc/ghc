{-# LANGUAGE ExplicitForAll, PolyKinds, DataKinds, TypeFamilies, GADTs #-}

module T19677 where

import GHC.Exts

class C x where
  meth :: x -> ()
instance C (a -> b) where
  meth _ = ()

type family Lifty throttle where
  Lifty Int = LiftedRep

data G a where
  MkG :: G Int

foo :: forall i (a :: TYPE (Lifty i)) (b :: TYPE (Lifty i)). G i -> (a -> b) -> ()
foo MkG = meth
