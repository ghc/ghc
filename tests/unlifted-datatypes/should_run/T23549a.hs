{-# LANGUAGE GADTs, UnliftedDatatypes #-}
module T23549a where

import GHC.Exts (UnliftedType)
import GHC.Types (Type)

data UnliftedGADTProxy :: Type -> UnliftedType where
  UGPInt :: UnliftedGADTProxy Int
  UGPBool :: UnliftedGADTProxy Bool
