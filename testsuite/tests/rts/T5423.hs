
{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

import GHC.Exts

foreign import prim "test" test :: Int# -> Int# -> Int# -> Int# -> Int#
                                -> Int# -> Int# -> Int# -> Int# -> Int#
                                -> Int#

foreign import ccall "flush_stdout" flush_stdout :: IO ()

v :: Int
v = I# (test 111# 112# 113# 114# 115# 116# 117# 118# 119# 120#)

main :: IO ()
main = do
  n <- return $! v
  flush_stdout -- Ensure that libc output buffer is flushed
  print n
