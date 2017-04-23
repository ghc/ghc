{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.IO.Handle.Lock (
    FileLockingNotSupported(..)
  , LockMode(..)
  , hLock
  , hTryLock
  ) where

#include "HsBaseConfig.h"

#if HAVE_FLOCK

#include <sys/file.h>

import Data.Bits
import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import GHC.IO.Exception
import GHC.IO.FD
import GHC.IO.Handle.FD

#elif defined(mingw32_HOST_OS)

#if defined(i386_HOST_ARCH)
## define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
## define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

#include <windows.h>

import Data.Bits
import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import GHC.IO.FD
import GHC.IO.Handle.FD
import GHC.Ptr
import GHC.Windows

#else

import GHC.IO (throwIO)

#endif

import Data.Functor
import GHC.Base
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.Show

-- | Exception thrown by 'hLock' on non-Windows platforms that don't support
-- 'flock'.
data FileLockingNotSupported = FileLockingNotSupported
  deriving Show

instance Exception FileLockingNotSupported

-- | Indicates a mode in which a file should be locked.
data LockMode = SharedLock | ExclusiveLock

-- | If a 'Handle' references a file descriptor, attempt to lock contents of the
-- underlying file in appropriate mode. If the file is already locked in
-- incompatible mode, this function blocks until the lock is established. The
-- lock is automatically released upon closing a 'Handle'.
--
-- Things to be aware of:
--
-- 1) This function may block inside a C call. If it does, in order to be able
-- to interrupt it with asynchronous exceptions and/or for other threads to
-- continue working, you MUST use threaded version of the runtime system.
--
-- 2) The implementation uses 'LockFileEx' on Windows and 'flock' otherwise,
-- hence all of their caveats also apply here.
--
-- 3) On non-Windows plaftorms that don't support 'flock' (e.g. Solaris) this
-- function throws 'FileLockingNotImplemented'. We deliberately choose to not
-- provide fcntl based locking instead because of its broken semantics.
--
-- @since 4.10.0.0
hLock :: Handle -> LockMode -> IO ()
hLock h mode = void $ lockImpl h "hLock" mode True

-- | Non-blocking version of 'hLock'.
--
-- @since 4.10.0.0
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = lockImpl h "hTryLock" mode False

----------------------------------------

#if HAVE_FLOCK

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  let flags = cmode .|. (if block then 0 else #{const LOCK_NB})
  fix $ \retry -> c_flock fd flags >>= \case
    0 -> return True
    _ -> getErrno >>= \errno -> if
      | not block && errno == eWOULDBLOCK -> return False
      | errno == eINTR -> retry
      | otherwise -> ioException $ errnoToIOError ctx errno (Just h) Nothing
  where
    cmode = case mode of
      SharedLock    -> #{const LOCK_SH}
      ExclusiveLock -> #{const LOCK_EX}

foreign import ccall interruptible "flock"
  c_flock :: CInt -> CInt -> IO CInt

#elif defined(mingw32_HOST_OS)

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) ctx $ c_get_osfhandle fd
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    let flags = cmode .|. (if block then 0 else #{const LOCKFILE_FAIL_IMMEDIATELY})
    -- We want to lock the whole file without looking up its size to be
    -- consistent with what flock does. According to documentation of LockFileEx
    -- "locking a region that goes beyond the current end-of-file position is
    -- not an error", however some versions of Windows seem to have issues with
    -- large regions and set ERROR_INVALID_LOCK_RANGE in such case for
    -- mysterious reasons. Work around that by setting only low 32 bits.
    fix $ \retry -> c_LockFileEx wh flags 0 0xffffffff 0xffffffff ovrlpd >>= \case
      True  -> return True
      False -> getLastError >>= \err -> if
        | not block && err == #{const ERROR_LOCK_VIOLATION} -> return False
        | err == #{const ERROR_OPERATION_ABORTED} -> retry
        | otherwise -> failWith ctx err
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

    cmode = case mode of
      SharedLock    -> 0
      ExclusiveLock -> #{const LOCKFILE_EXCLUSIVE_LOCK}

-- https://msdn.microsoft.com/en-us/library/aa297958.aspx
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365203.aspx
foreign import WINDOWS_CCONV interruptible "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL

#else

-- | No-op implementation.
lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl _ _ _ _ = throwIO FileLockingNotSupported

#endif
