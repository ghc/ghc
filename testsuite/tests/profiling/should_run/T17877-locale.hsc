{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Exception
import Foreign.C.Types
import Foreign.C.String

#include <locale.h>
lcAll = #const LC_ALL
foreign import ccall "locale.h setlocale" c_setlocale :: CInt -> CString -> IO ()
foreign import ccall  "stdio.h printf"    c_printf :: CString -> CDouble -> IO ()

main = do
  withCString "fr_FR.utf8" $ c_setlocale lcAll

  -- allocate some junk to get a heap profile
  s <- getLine
  evaluate $ last $ scanl1 (+) $ map fromEnum $ concat $ replicate 100000 s
  -- check 0 (test validity): ensure we got a profile (sed|grep in Makefile)
      
  -- check 1: RTS doesn't override user's locale setting
  withCString "%'.3f\n" $ \fmt -> c_printf fmt (read s)

  -- check 2: heap profile is readable by hp2ps (hp2ps in Makefile)
