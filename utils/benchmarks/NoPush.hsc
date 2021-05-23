{-# LANGUAGE ForeignFunctionInterface #-}

module NoPush (setNoPush) where

#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)
import Network.Socket (Socket, withFdSocket)

noPush :: CInt
#if defined(TCP_NOPUSH)
noPush = #const TCP_NOPUSH
#elif defined(TCP_CORK)
noPush = #const TCP_CORK
#else
noPush = 0
#endif

setNoPush :: Socket -> Bool -> IO ()
setNoPush _ _ | noPush == 0 = return ()
setNoPush sock onOff =
  withFdSocket sock $ \fd -> do
     let v = if onOff then 1 else 0
     with v $ \ptr ->
        throwErrnoIfMinus1_ "setNoPush" $
        c_setsockopt fd (#const IPPROTO_TCP) noPush ptr (fromIntegral (sizeOf v))

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
