{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Main where

import GHC.Int
import GHC.Prim

foreign import prim "test" test :: Int# -> Int#
foreign import prim "test2" test2 :: Int# -> Int#
foreign import prim "test3" test3 :: Int# -> Int#

main = print (I# (test3 (test2 (test 0#))))
