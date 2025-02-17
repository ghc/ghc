{-# LANGUAGE CApiFFI #-}
module T7388 where

import Foreign.C
import Foreign.Ptr

foreign import capi "stdio.h printf" printfb :: CString -> CInt -> IO ()
foreign import capi "stdio.h fflush" fflushb :: Ptr () -> IO ()
