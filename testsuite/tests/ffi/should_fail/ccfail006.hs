{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, MagicHash #-}

module Ccfail006 where

import GHC.Exts

foreign import prim "foo" foo :: Bool -> Int#
foreign import prim "bar" bar :: Int# -> Bool
