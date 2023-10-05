{-# LANGUAGE UnliftedDatatypes #-}

module UnlDataInvalidResKind1 where

import GHC.Exts
import GHC.Types

type T :: Type -> TYPE IntRep
data T a = MkT Int
