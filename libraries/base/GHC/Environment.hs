{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Environment (getFullArgs) where

import Foreign
import Foreign.C
import GHC.Base
import GHC.Real ( fromIntegral )
import GHC.IO.Encoding
import qualified GHC.Foreign as GHC

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

-- | Computation 'getFullArgs' is the "raw" version of
-- 'System.Environment.getArgs', similar to @argv@ in other languages. It
-- returns a list of the program's command line arguments, starting with the
-- program name, and including those normally eaten by the RTS (+RTS ... -RTS).
getFullArgs :: IO [String]
getFullArgs =
  alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        getFullProgArgv p_argc p_argv
        p    <- fromIntegral `liftM` peek p_argc
        argv <- peek p_argv
        enc <- argvEncoding
        peekArray p argv >>= mapM (GHC.peekCString enc)

foreign import ccall unsafe "getFullProgArgv"
    getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
