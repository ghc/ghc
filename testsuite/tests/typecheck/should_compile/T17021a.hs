{-# LANGUAGE StandaloneKindSignatures, PolyKinds, DataKinds, TypeFamilies,
             UnliftedNewtypes #-}

module T17021a where

import Data.Kind
import GHC.Exts

type family Id x where
  Id x = x

type LevId :: TYPE (Id LiftedRep) -> TYPE (Id LiftedRep)
newtype LevId x = MkLevId x

type LevId2 :: (r ~ Id LiftedRep) => TYPE r -> TYPE r
newtype LevId2 x = MkLevId2 x
