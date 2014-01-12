
{-# LANGUAGE TypeFamilies, GHCForeignImportPrim, MagicHash,
             UnliftedFFITypes #-}

module Cc015 where

import Foreign
import Foreign.C.Types
import GHC.Prim

type family F a
type instance F Int = Int# -> Int#

foreign import prim "f" f :: F Int

