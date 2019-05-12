{-# LANGUAGE ExplicitForAll, PolyKinds, TypeFamilies, GADTs, UnliftedNewtypes #-}

module UnliftedNewtypesLPFamily where

import GHC.Exts

data family DF (a :: k) :: k

newtype instance DF (a :: TYPE r) where
  MkDF :: forall (r :: RuntimeRep) (a :: TYPE r). a -> DF a
