{-# LANGUAGE CPP #-}
module GHCi.Utils
  ( getGhcHandle
  , readGhcHandle
  , readGhcHandleWineRead
  , readGhcHandleWineWrite
  , addWineLibrarySearchPath
  , ensureMsvcrtAssertShim
  )
where

import Prelude
import Foreign.C (CInt(..))
import GHC.IO.Handle (Handle())
#if defined(mingw32_HOST_OS)
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Char (isAlpha, toUpper)
import Data.List (intercalate)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, ptrToIntPtr, wordPtrToPtr)
import Foreign.Storable (peek)
import GHC.IO (onException)
import GHC.IO.Handle.FD (fdToHandle)
import GHC.Windows (HANDLE, DWORD, NTSTATUS)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.IOMode
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle())
# if defined(__IO_MANAGER_WINIO__)
import GHC.Event.Windows (associateHandle')
# endif
import System.Environment (lookupEnv, setEnv)
import System.Win32.DLL (getModuleHandle, getProcAddress, loadLibrary)
import System.Win32.Types (Addr, HMODULE)

#include <windows.h>
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

#if defined(mingw32_HOST_OS)
addWineLibrarySearchPath :: String -> IO ()
addWineLibrarySearchPath path = do
  let winPath = toWinePath path
  unless (null winPath) $ do
    cur <- lookupEnv "PATH"
    let paths = maybe [] splitSemi cur
        paths' =
          if any (eqPath winPath) paths
            then paths
            else paths ++ [winPath]
    setEnv "PATH" (intercalate ";" paths')
  where
    toWinePath p
      | isWindowsPath p = normalize p
      | isPosixAbs p    = "Z:" ++ normalize p
      | otherwise       = normalize p
    normalize = map slash
    slash '/' = '\\'
    slash c   = c
    isPosixAbs ('/':_) = True
    isPosixAbs _       = False
    isWindowsPath (c:':':_) | isAlpha c = True
    isWindowsPath ('\\':'\\':_) = True
    isWindowsPath _ = False
    splitSemi s' = case break (== ';') s' of
      (a, ';':rest) -> a : splitSemi rest
      (a, _)        -> [a]
    eqPath a b = map toUpper a == map toUpper b

ensureMsvcrtAssertShim :: IO ()
ensureMsvcrtAssertShim = ghciMsvcrtAssertShim

foreign import ccall unsafe "ghci_msvcrt_assert_shim"
  ghciMsvcrtAssertShim :: IO ()


readGhcHandleWineRead :: String -> IO Handle
readGhcHandleWineRead = readGhcHandleWine wineReadAccess

readGhcHandleWineWrite :: String -> IO Handle
readGhcHandleWineWrite = readGhcHandleWine wineWriteAccess

readGhcHandleWine :: DWORD -> String -> IO Handle
readGhcHandleWine access s = do
  let fd = Prelude.read s :: CInt
  handle <- wineServerFdToHandle fd access
# if defined(__IO_MANAGER_WINIO__)
  return () <!> associateHandle' handle
# endif
  getGhcHandle handle

wineServerFdToHandle :: CInt -> DWORD -> IO HANDLE
wineServerFdToHandle fd access = do
  mwine <- getWineServerFdToHandle
  case mwine of
    Nothing -> fail "wine_server_fd_to_handle not found in ntdll.dll"
    Just f -> alloca $ \out -> do
      status <- f fd access 0 out
      if status /= 0
        then fail ("wine_server_fd_to_handle failed with status " ++ show status)
        else peek out

type WineServerFdToHandle = CInt -> DWORD -> DWORD -> Ptr HANDLE -> IO NTSTATUS

wineReadAccess :: DWORD
wineReadAccess = (#const GENERIC_READ) .|. (#const SYNCHRONIZE)

wineWriteAccess :: DWORD
wineWriteAccess = (#const GENERIC_WRITE) .|. (#const SYNCHRONIZE)

getWineServerFdToHandle :: IO (Maybe WineServerFdToHandle)
getWineServerFdToHandle = do
  mhmod <- getNtdllModule
  case mhmod of
    Nothing -> pure Nothing
    Just hmod -> do
      maddr <- getProc "wine_server_fd_to_handle" hmod
      pure (mkWineServerFdToHandle . castPtrToFunPtr <$> maddr)

getNtdllModule :: IO (Maybe HMODULE)
getNtdllModule = do
  m1 <- try (getModuleHandle (Just "ntdll.dll")) :: IO (Either IOException HMODULE)
  case m1 of
    Right hmod -> pure (Just hmod)
    Left _ -> do
      m2 <- try (loadLibrary "ntdll.dll") :: IO (Either IOException HMODULE)
      case m2 of
        Right hmod -> pure (Just hmod)
        Left _ -> pure Nothing

getProc :: String -> HMODULE -> IO (Maybe Addr)
getProc name hmod = do
  res <- try (getProcAddress hmod name) :: IO (Either IOException Addr)
  case res of
    Right addr -> pure (Just addr)
    Left _ -> pure Nothing

foreign import ccall "dynamic"
  mkWineServerFdToHandle :: FunPtr WineServerFdToHandle -> WineServerFdToHandle
#else
readGhcHandleWineRead :: String -> IO Handle
readGhcHandleWineRead = readGhcHandle

readGhcHandleWineWrite :: String -> IO Handle
readGhcHandleWineWrite = readGhcHandle

addWineLibrarySearchPath :: String -> IO ()
addWineLibrarySearchPath _ = pure ()

ensureMsvcrtAssertShim :: IO ()
ensureMsvcrtAssertShim = pure ()

#endif
