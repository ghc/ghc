
{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

import GHC.Exts

foreign import prim "test" test :: Int# -> Int# -> Int# -> Int# -> Int#
                                -> Int# -> Int# -> Int# -> Int# -> Int#
                                -> Int#

v :: Int
v = I# (test 111# 112# 113# 114# 115# 116# 117# 118# 119# 120#)

main :: IO ()
main = print v
