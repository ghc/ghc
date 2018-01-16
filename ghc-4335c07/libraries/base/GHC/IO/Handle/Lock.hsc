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
  , hUnlock
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

-- | Release a lock taken with 'hLock' or 'hTryLock'.
hUnlock :: Handle -> IO ()
hUnlock = unlockImpl

----------------------------------------

#if HAVE_OFD_LOCKING
-- Linux open file descriptor locking.
--
-- We prefer this over BSD locking (e.g. flock) since the latter appears to
-- break in some NFS configurations. Note that we intentionally do not try to
-- use ordinary POSIX file locking due to its peculiar semantics under
-- multi-threaded environments.

foreign import ccall interruptible "fcntl"
  c_fcntl :: CInt -> CInt -> Ptr () -> IO CInt

data FLock  = FLock { l_type   :: CShort
                    , l_whence :: CShort
                    , l_start  :: COff
                    , l_len    :: COff
                    , l_pid    :: CPid
                    }

instance Storable FLock where
    sizeOf _ = #{size flock}
    alignment _ = #{alignment flock}
    poke ptr x = do
        fillBytes ptr 0 (sizeOf x)
        #{poke flock, l_type}   ptr (l_type x)
        #{poke flock, l_whence} ptr (l_whence x)
        #{poke flock, l_start}  ptr (l_start x)
        #{poke flock, l_len}    ptr (l_len x)
        #{poke flock, l_pid}    ptr (l_pid x)
    peek ptr = do
        FLock <$> #{peek flock, l_type}   ptr
              <*> #{peek flock, l_whence} ptr
              <*> #{peek flock, l_start}  ptr
              <*> #{peek flock, l_len}    ptr
              <*> #{peek flock, l_pid}    ptr

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  with flock $ \flock_ptr -> fix $ \retry -> do
      ret <- with flock $ fcntl fd mode flock_ptr
      case ret of
        0 -> return True
        _ -> getErrno >>= \errno -> if
          | not block && errno == eWOULDBLOCK -> return False
          | errno == eINTR -> retry
          | otherwise -> ioException $ errnoToIOError ctx errno (Just h) Nothing
  where
    flock = FLock { l_type = case mode of
                               SharedLock -> #{const F_RDLCK}
                               ExclusiveLock -> #{const F_WRLCK}
                  , l_whence = #{const SEEK_SET}
                  , l_start = 0
                  , l_len = 0
                  }
    mode
      | block     = #{const F_SETLKW}
      | otherwise = #{const F_SETLK}

unlockImpl :: Handle -> IO ()
unlockImpl h = do
  FD{fdFD = fd} <- handleToFd h
  let flock = FLock { l_type = #{const F_UNLCK}
                    , l_whence = #{const SEEK_SET}
                    , l_start = 0
                    , l_len = 0
                    }
  throwErrnoIfMinus1_ "hUnlock"
      $ with flock $ c_fcntl fd #{const F_SETLK}

#elif HAVE_FLOCK

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  let flags = cmode .|. (if block then 0 else #{const LOCK_NB})
  fix $ \retry -> c_flock fd flags >>= \case
    0 -> return True
    _ -> getErrno >>= \errno -> if
      | not block
      , errno == eAGAIN || errno == eACCES -> return False
      | errno == eINTR -> retry
      | otherwise -> ioException $ errnoToIOError ctx errno (Just h) Nothing
  where
    cmode = case mode of
      SharedLock    -> #{const LOCK_SH}
      ExclusiveLock -> #{const LOCK_EX}

unlockImpl :: Handle -> IO ()
unlockImpl h = do
  FD{fdFD = fd} <- handleToFd h
  throwErrnoIfMinus1_ "flock" $ c_flock fd #{const LOCK_UN}

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
    -- not an error", hence we pass maximum value as the number of bytes to
    -- lock.
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

unlockImpl :: Handle -> IO ()
unlockImpl h = do
  FD{fdFD = fd} <- handleToFd h
  wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) "hUnlock" $ c_get_osfhandle fd
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    c_UnlockFileEx wh 0 0xffffffff 0xffffffff ovrlpd >>= \case
      True  -> return ()
      False -> getLastError >>= failWith "hUnlock"
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

-- https://msdn.microsoft.com/en-us/library/aa297958.aspx
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365203.aspx
foreign import WINDOWS_CCONV interruptible "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365716.aspx
foreign import WINDOWS_CCONV interruptible "UnlockFileEx"
  c_UnlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL

#else

-- | No-op implementation.
lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl _ _ _ _ = throwIO FileLockingNotSupported

-- | No-op implementation.
unlockImpl :: Handle -> IO ()
unlockImpl _ = throwIO FileLockingNotSupported

#endif
