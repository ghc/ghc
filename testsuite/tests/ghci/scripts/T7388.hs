{-# LANGUAGE CApiFFI #-}
module T7388 where

import Foreign.C

foreign import capi "stdio.h printf" printfb :: CString -> CInt -> IO ()
