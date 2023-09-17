{-# LANGUAGE CPP #-}
module T23309A (c_foo) where

import Foreign.C.String
import Foreign.C.Types

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define CALLCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define CALLCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#else
# define CALLCONV ccall
#endif

foreign import CALLCONV unsafe "foo" c_foo :: CInt -> IO CString
