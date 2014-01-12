{-# LANGUAGE MagicHash,GHCForeignImportPrim,UnliftedFFITypes #-}
module Main where

import GHC.Exts

foreign import prim "f5149" f :: Int# -> Int# -> Double# -> Int#

main = print (I# (f 1# 2# 1.0##))
