{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnliftedFFITypes  #-}

module Main where

import qualified Data.ByteString.Short.Internal as SBS
import           Foreign.C.Types
import           GHC.Exts

foreign import capi  unsafe "string.h strlen"
    c_strlen_capi :: ByteArray# -> IO CSize

foreign import capi  unsafe "string.h memset"
    c_memset_capi :: MutableByteArray# s -> CInt -> CSize -> IO ()

main :: IO ()
main = do
    n <- c_strlen_capi ba#
    print (n == 13)
  where
    !(SBS.SBS ba#) = "Hello FFI!!!!\NUL"
