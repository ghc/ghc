{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash #-}
module Data.Typeable.Internal (
    TypeRep,
    TyCon,
    mkTyCon
  ) where

import GHC.Base

data TypeRep
data TyCon

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 64
mkTyCon :: Word64# -> Word64# -> String -> String -> String -> TyCon
#else
mkTyCon :: Word#   -> Word#   -> String -> String -> String -> TyCon
#endif
