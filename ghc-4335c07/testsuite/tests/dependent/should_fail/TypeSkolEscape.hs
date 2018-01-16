{-# LANGUAGE RankNTypes, PolyKinds, TypeInType #-}

module TypeSkolEscape where

import GHC.Types
import GHC.Exts

type Bad = forall (v :: RuntimeRep) (a :: TYPE v). a
