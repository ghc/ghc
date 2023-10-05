{-# LANGUAGE CPP #-}
module GHCi.Utils
  ( getGhcHandle
  , readGhcHandle
  )
where

import Prelude
import Foreign.C
import GHC.IO.Handle (Handle())
#if defined(mingw32_HOST_OS)
import Foreign.Ptr (ptrToIntPtr,wordPtrToPtr)
import GHC.IO (onException)
import GHC.IO.Handle.FD (fdToHandle)
import GHC.Windows (HANDLE)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.IOMode
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle())

#include <fcntl.h>     /* for _O_BINARY */

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
readGhcHandle :: String -> IO Handle
readGhcHandle s = do
#if defined(mingw32_HOST_OS)
  let fd = wordPtrToPtr (Prelude.read s)
# if defined(__IO_MANAGER_WINIO__)
  -- register the handles we received with
  -- our I/O manager otherwise we can't use
  -- them correctly.
  return () <!> associateHandle' fd
# endif
#else
  let fd = Prelude.read s
#endif
  getGhcHandle fd

