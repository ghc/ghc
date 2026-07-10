{-# LANGUAGE CPP #-}

module General.FileLock(usingLockFile) where

import Control.Exception.Extra
import System.FilePath
import General.Extra
import General.Cleanup
#ifdef mingw32_HOST_OS
import Control.Monad
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.C.String
#else
import System.IO
import System.Posix.IO
#endif

#ifdef mingw32_HOST_OS

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateFileW" c_CreateFileW :: CWString -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Ptr () -> IO (Ptr ())
foreign import CALLCONV unsafe "Windows.h CloseHandle" c_CloseHandle :: Ptr () -> IO Bool
foreign import CALLCONV unsafe "Windows.h GetLastError" c_GetLastError :: IO Word32

c_GENERIC_WRITE = 0x40000000 :: Word32
c_GENERIC_READ  = 0x80000000 :: Word32
c_FILE_SHARE_NONE = 0 :: Word32
c_OPEN_ALWAYS = 4 :: Word32
c_FILE_ATTRIBUTE_NORMAL = 0x80 :: Word32
c_INVALID_HANDLE_VALUE = intPtrToPtr (-1)
c_ERROR_SHARING_VIOLATION = 32
#endif

usingLockFile :: Cleanup -> FilePath -> IO ()

#ifdef mingw32_HOST_OS

usingLockFile b file = do
    createDirectoryRecursive $ takeDirectory file
    let open = withCWString file $ \cfile ->
            c_CreateFileW cfile (c_GENERIC_READ .|. c_GENERIC_WRITE) c_FILE_SHARE_NONE nullPtr c_OPEN_ALWAYS c_FILE_ATTRIBUTE_NORMAL nullPtr
    h <- allocate b open (void . c_CloseHandle)
    when (h == c_INVALID_HANDLE_VALUE) $ do
        err <- c_GetLastError
        errorIO $ "Shake failed to acquire a file lock on " ++ file ++ "\n" ++
                    (if err == c_ERROR_SHARING_VIOLATION
                        then "ERROR_SHARING_VIOLATION - Shake is probably already running."
                        else "Code " ++ show err ++ ", unknown reason for failure.")

#else

usingLockFile cleanup file = do
    createDirectoryRecursive $ takeDirectory file
    tryIO $ writeFile file ""

    fd <- allocate cleanup (openSimpleFd file ReadWrite) closeFd
    let lock = (WriteLock, AbsoluteSeek, 0, 0)
    setLock fd lock `catchIO` \e -> do
        res <- getLock fd lock
        errorIO $ "Shake failed to acquire a file lock on " ++ file ++ "\n" ++
                    (case res of
                        Nothing -> ""
                        Just (pid, _) -> "Shake process ID " ++ show pid ++ " is using this lock.\n") ++
                    show e

#ifndef MIN_VERSION_unix
#define MIN_VERSION_unix(a,b,c) 0
#endif
#if MIN_VERSION_unix(2,8,0)
openSimpleFd file mode = openFd file mode defaultFileFlags
#else
openSimpleFd file mode = openFd file mode Nothing defaultFileFlags
#endif

#endif
