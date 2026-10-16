{-# LANGUAGE CPP #-}
module GHCi.Utils
  ( getGhcHandle
  , readGhcHandle
  )
where

import Prelude
import Foreign.C
import GHC.IO.Handle (Handle())
import GHC.IO.IOMode
#if defined(mingw32_HOST_OS)
import Foreign.Ptr (ptrToIntPtr)
import GHC.IO (onException)
import GHC.IO.Handle.FD (fdToHandle)
import GHC.Windows (BOOL, DWORD, HANDLE, failIfFalse_)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle())
import System.Process.CommunicationHandle.Internal
  ( CommunicationHandle(..), useCommunicationHandle )

#include <fcntl.h>     /* for _O_BINARY */
#include <windows.h>

#else
import System.Posix
#endif

-- | Gets a GHC Handle File description from the given OS Handle or POSIX fd.

#if defined(mingw32_HOST_OS)
getGhcHandle :: HANDLE -> IO Handle
getGhcHandle = getGhcHandlePOSIX <!> getGhcHandleNative

getGhcHandlePOSIX :: HANDLE -> IO Handle
getGhcHandlePOSIX handle = do
  let intptr = ptrToIntPtr handle
  _open_osfhandle (fromIntegral intptr) (#const _O_BINARY) >>= fdToHandle

getGhcHandleNative :: HANDLE -> IO Handle
getGhcHandleNative hwnd =
  do mb_codec <- fmap Just getLocaleEncoding
     let iomode = ReadWriteMode
         native_handle = fromHANDLE hwnd :: Io NativeHandle
     hw_type <- IODevice.devType $ native_handle
     mkHandleFromHANDLE native_handle hw_type (show hwnd) iomode mb_codec
       `onException` IODevice.close native_handle

foreign import ccall "io.h _open_osfhandle" _open_osfhandle ::
    CInt -> CInt -> IO CInt
#else
getGhcHandle :: CInt -> IO Handle
getGhcHandle fd     = fdToHandle $ Fd fd
#endif

-- | Read a handle passed on the command-line and prepare it to be used with the IO manager
-- also make the handle non-inheritable so we don't have additional references floating
-- around that stop reads from unblocking on close
readGhcHandle :: IOMode -> String -> IO Handle
#if defined(mingw32_HOST_OS)
readGhcHandle mode s = do
  let ch@(CommunicationHandle h) = Prelude.read s
  failIfFalse_ "SetHandleInformation" $
    c_SetHandleInformation h (#const HANDLE_FLAG_INHERIT) 0
  useCommunicationHandle (mode == ReadMode) ch

foreign import ccall unsafe "windows.h SetHandleInformation"
  c_SetHandleInformation :: HANDLE -> DWORD -> DWORD -> IO BOOL
#else
readGhcHandle _ s = do
  let fd = Prelude.read s
# if !defined(javascript_HOST_ARCH)
  setFdOption (Fd fd) CloseOnExec True
# endif
  getGhcHandle fd
#endif
