{-# LANGUAGE StandaloneKindSignatures, PolyKinds, DataKinds, TypeFamilies,
             UnliftedNewtypes #-}

module T17021a where

import Data.Kind
import GHC.Exts

type family Id x where
  Id x = x

type LevId :: TYPE (Id LiftedRep) -> TYPE (Id LiftedRep)
newtype LevId x = MkLevId x
