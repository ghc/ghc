{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, MagicHash #-}

module Lib where

import GHC.Exts

foreign import prim f1 :: Int# -> Int#
