{-# LANGUAGE MagicHash,GHCForeignImportPrim,UnliftedFFITypes #-}
module Main where

import GHC.Exts
import Control.Exception
import System.IO

foreign import prim "memintrinTest" basicTest :: Int# -> Int#

foreign import prim "testMemset8_0" testMemset8_0 :: Int# -> Int#
foreign import prim "testMemset8_8" testMemset8_8 :: Int# -> Int#
foreign import prim "testMemset8_9" testMemset8_9 :: Int# -> Int#
foreign import prim "testMemset8_10" testMemset8_10 :: Int# -> Int#
foreign import prim "testMemset8_11" testMemset8_11 :: Int# -> Int#
foreign import prim "testMemset8_12" testMemset8_12 :: Int# -> Int#
foreign import prim "testMemset8_13" testMemset8_13 :: Int# -> Int#
foreign import prim "testMemset8_14" testMemset8_14 :: Int# -> Int#
foreign import prim "testMemset8_15" testMemset8_15 :: Int# -> Int#
foreign import prim "testMemset8_16" testMemset8_16 :: Int# -> Int#
foreign import prim "testMemset4_0" testMemset4_0 :: Int# -> Int#
foreign import prim "testMemset4_4" testMemset4_4 :: Int# -> Int#
foreign import prim "testMemset4_5" testMemset4_5 :: Int# -> Int#
foreign import prim "testMemset4_6" testMemset4_6 :: Int# -> Int#
foreign import prim "testMemset4_7" testMemset4_7 :: Int# -> Int#
foreign import prim "testMemset4_8" testMemset4_8 :: Int# -> Int#

foreign import prim "testMemcpy8_0" testMemcpy8_0 :: Int# -> Int#
foreign import prim "testMemcpy8_8" testMemcpy8_8 :: Int# -> Int#
foreign import prim "testMemcpy8_9" testMemcpy8_9 :: Int# -> Int#
foreign import prim "testMemcpy8_10" testMemcpy8_10 :: Int# -> Int#
foreign import prim "testMemcpy8_11" testMemcpy8_11 :: Int# -> Int#
foreign import prim "testMemcpy8_12" testMemcpy8_12 :: Int# -> Int#
foreign import prim "testMemcpy8_13" testMemcpy8_13 :: Int# -> Int#
foreign import prim "testMemcpy8_14" testMemcpy8_14 :: Int# -> Int#
foreign import prim "testMemcpy8_15" testMemcpy8_15 :: Int# -> Int#
foreign import prim "testMemcpy8_16" testMemcpy8_16 :: Int# -> Int#
foreign import prim "testMemcpy4_0" testMemcpy4_0 :: Int# -> Int#
foreign import prim "testMemcpy4_4" testMemcpy4_4 :: Int# -> Int#
foreign import prim "testMemcpy4_5" testMemcpy4_5 :: Int# -> Int#
foreign import prim "testMemcpy4_6" testMemcpy4_6 :: Int# -> Int#
foreign import prim "testMemcpy4_7" testMemcpy4_7 :: Int# -> Int#
foreign import prim "testMemcpy4_8" testMemcpy4_8 :: Int# -> Int#

main = do
    putStrLn "Mem{cpy,set,move} Intrinsics Test..."
    _ <- evaluate (I# (basicTest 1#))
    
    _ <- evaluate (I# (testMemset8_0 1#))
    _ <- evaluate (I# (testMemset8_8 1#))
    _ <- evaluate (I# (testMemset8_9 1#))
    _ <- evaluate (I# (testMemset8_10 1#))
    _ <- evaluate (I# (testMemset8_11 1#))
    _ <- evaluate (I# (testMemset8_12 1#))
    _ <- evaluate (I# (testMemset8_13 1#))
    _ <- evaluate (I# (testMemset8_14 1#))
    _ <- evaluate (I# (testMemset8_15 1#))
    _ <- evaluate (I# (testMemset8_16 1#))
    _ <- evaluate (I# (testMemset4_0 1#))
    _ <- evaluate (I# (testMemset4_4 1#))
    _ <- evaluate (I# (testMemset4_5 1#))
    _ <- evaluate (I# (testMemset4_6 1#))
    _ <- evaluate (I# (testMemset4_7 1#))
    _ <- evaluate (I# (testMemset4_8 1#))
    
    _ <- evaluate (I# (testMemcpy8_0 1#))
    _ <- evaluate (I# (testMemcpy8_8 1#))
    _ <- evaluate (I# (testMemcpy8_9 1#))
    _ <- evaluate (I# (testMemcpy8_10 1#))
    _ <- evaluate (I# (testMemcpy8_11 1#))
    _ <- evaluate (I# (testMemcpy8_12 1#))
    _ <- evaluate (I# (testMemcpy8_13 1#))
    _ <- evaluate (I# (testMemcpy8_14 1#))
    _ <- evaluate (I# (testMemcpy8_15 1#))
    _ <- evaluate (I# (testMemcpy8_16 1#))
    _ <- evaluate (I# (testMemcpy4_0 1#))
    _ <- evaluate (I# (testMemcpy4_4 1#))
    _ <- evaluate (I# (testMemcpy4_5 1#))
    _ <- evaluate (I# (testMemcpy4_6 1#))
    _ <- evaluate (I# (testMemcpy4_7 1#))
    _ <- evaluate (I# (testMemcpy4_8 1#))
    putStrLn "Test Passed!"
    return ()
