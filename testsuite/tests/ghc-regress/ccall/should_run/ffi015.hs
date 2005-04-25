{-# OPTIONS_GHC -fffi -#include "ffi015_cbits.h" #-}

import Foreign
import Foreign.C

foreign import ccall "&var" var :: Ptr CInt

main = do
  x <- peek var
  print x

