{-# OPTIONS_GHC -#include "ffi015_cbits.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C

foreign import ccall "&var" var :: Ptr CInt

main = do
  x <- peek var
  print x

