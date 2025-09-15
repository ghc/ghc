{-# LANGUAGE CPP #-}
module T23378A where

import Foreign.C.Types
import System.IO.Unsafe

isatty :: Bool
isatty =
  unsafePerformIO (c_isatty 1) == 1
{-# NOINLINE isatty #-}

#if defined(mingw32_HOST_OS)
# define SYM "_isatty"
#else
# define SYM "isatty"
#endif

foreign import ccall unsafe SYM
  c_isatty :: CInt -> IO CInt
