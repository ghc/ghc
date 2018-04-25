
{-# LANGUAGE CApiFFI #-}

module T4012_A where

import Foreign.C

a :: IO ()
a = do withCString "Double: %f\n" $ \fstr -> printfa1 fstr 1.5
       withCString "Int: %d\n"    $ \fstr -> printfa2 fstr 4

foreign import capi "stdio.h printf" printfa1 :: CString -> CDouble -> IO ()
foreign import capi "stdio.h printf" printfa2 :: CString -> CInt -> IO ()

