{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, MagicHash #-}

module Lib where

import GHC.Prim

foreign import prim f1 :: Int# -> Int#
