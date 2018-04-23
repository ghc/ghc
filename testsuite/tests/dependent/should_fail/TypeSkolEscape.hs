{-# LANGUAGE RankNTypes, PolyKinds, TypeInType #-}
-- NB: -fprint-explicit-runtime-reps enabled in all.T

module TypeSkolEscape where

import GHC.Types
import GHC.Exts

type Bad = forall (v :: RuntimeRep) (a :: TYPE v). a
