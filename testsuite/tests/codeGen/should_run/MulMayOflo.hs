{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnliftedFFITypes #-}

module Main where
import GHC.Exts

foreign import prim "runCmmzh" runCmm# :: Int# -> Int#

main :: IO ()
main = (print . show) (I# (runCmm# 0#))
