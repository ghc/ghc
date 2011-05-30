{-# LANGUAGE MagicHash,GHCForeignImportPrim,UnliftedFFITypes #-}
module Main where

import GHC.Exts
import Control.Exception
import System.IO

foreign import prim "memintrinTest" memcpyTest :: Int# -> Int#

main = do
    putStrLn "Mem{cpy,set,move} Intrinsics Test..."
    _ <- evaluate (I# (memcpyTest 1#))
    putStrLn "Test Passed!"
    return ()

