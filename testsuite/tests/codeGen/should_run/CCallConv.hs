{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnboxedTuples, UnliftedFFITypes #-}
module Main where

import GHC.Exts

foreign import prim "runCmmzh"
   runCmm# :: Word# -> Word#


main :: IO ()
main = print $ W# (runCmm# 0##)
