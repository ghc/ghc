{-# LANGUAGE MagicHash,GHCForeignImportPrim,UnliftedFFITypes #-}
module Main where

import GHC.Exts

foreign import prim "do_popcnt32" popcnt32 :: Int# -> Int#

main = print (I# (popcnt32 0xffff#))

