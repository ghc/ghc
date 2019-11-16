{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Test where

import Control.Concurrent
import Control.Monad
import Foreign.C

-- See also conc059_c.c
--
-- This test fires off some threads that will return after the RTS has
-- shut down.  This should not crash or confuse the RTS.

f :: Int -> IO ()
f x = do
  print x
  replicateM_ 10 $ forkIO $ do usleep (fromIntegral x); putStrLn "hello"
  return ()

foreign export ccall "f" f :: Int -> IO ()

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

foreign import WINDOWS_CCONV safe "Sleep" _sleep :: Int -> IO ()
usleep n = _sleep (n `quot` 1000)
#else
foreign import ccall safe "usleep" usleep  :: Int -> IO ()
#endif
