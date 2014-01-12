
{-# LANGUAGE CApiFFI #-}

module T4012_B where

import Foreign.C

b :: IO ()
b = withCString "Int: %d\n" $ \fstr -> printfb fstr 9

foreign import capi "stdio.h printf" printfb :: CString -> CInt -> IO ()
