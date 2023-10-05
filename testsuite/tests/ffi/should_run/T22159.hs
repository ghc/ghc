{-# LANGUAGE CPP #-}
module Main (main) where

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

import Foreign.C.String (peekCWString)
import Foreign.C.Types (CWchar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)

foreign import WINDOWS_CCONV "hello" c_hello :: Ptr CWchar -> IO ()

main :: IO ()
main = allocaBytes 12 $ \buf -> do
  c_hello buf
  str <- peekCWString buf
  putStrLn str
