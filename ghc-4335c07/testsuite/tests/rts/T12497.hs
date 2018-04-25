{-# LANGUAGE CPP #-}

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

import Foreign.C.String

foreign import WINDOWS_CCONV "_strdup" strdup :: CString -> IO CString
foreign import WINDOWS_CCONV "strdup" strdup2 :: CString -> IO CString

dupString :: String -> IO String
dupString str = newCString str >>= strdup >>= peekCString

dupString2 :: String -> IO String
dupString2 str = newCString str >>= strdup2 >>= peekCString

main =
 do print =<< dupString  "Hello World!"
    print =<< dupString2 "Hello Again World!"
