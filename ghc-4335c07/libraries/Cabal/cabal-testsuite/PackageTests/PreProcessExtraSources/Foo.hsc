{-# LANGUAGE ForeignFunctionInterface #-}
module Foo where

import Foreign.C.Types

#def int incr(int x) { return x + 1; }

foreign import ccall unsafe "Foo_hsc.h incr"
  incr :: CInt -> CInt
