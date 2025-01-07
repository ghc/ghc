{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle.Lock (
    FileLockingNotSupported(..)
  , LockMode(..)
  , hLock
  , hTryLock
  , hUnlock
  ) where


#include "HsBaseConfig.h"

import GHC.Internal.Data.Functor (void)
import GHC.Internal.Base
import GHC.Internal.IO.Handle.Lock.Common (LockMode(..), FileLockingNotSupported(..))
import GHC.Internal.IO.Handle.Types (Handle)

#if defined(mingw32_HOST_OS)
import GHC.Internal.IO.Handle.Lock.Windows
#elif HAVE_OFD_LOCKING
import GHC.Internal.IO.Handle.Lock.LinuxOFD
#elif HAVE_FLOCK
import GHC.Internal.IO.Handle.Lock.Flock
#else
import GHC.Internal.IO.Handle.Lock.NoOp
#endif

-- | If a 'Handle' references a file descriptor, attempt to lock contents of the
-- underlying file in appropriate mode. If the file is already locked in
-- incompatible mode, this function blocks until the lock is established. The
-- lock is automatically released upon closing a 'Handle'.
--
-- Things to be aware of:
--
-- 1. This function may block inside a C call. If it does, in order to be able
-- to interrupt it with asynchronous exceptions and/or for other threads to
-- continue working, you MUST use threaded version of the runtime system.
--
-- 2. The implementation uses relies on any of a number of locking
-- facilities, depending upon what the platform supports:
--
--   * 'LockFileEx' is used on Windows
--   * On platforms that support it we use the @F_OFD_SETLK@ and @F_OFD_SETLKW@ @fnctl@s.
--   * Otherwise we use @flock@
--
-- hence all of their caveats also apply here.
--
-- 3. On non-Windows platforms that don't support 'flock' (e.g. Solaris) this
-- function throws 'FileLockingNotImplemented'. We deliberately choose to not
-- provide fcntl based locking instead because of its broken semantics.
--
-- @since base-4.10.0.0
hLock :: Handle -> LockMode -> IO ()
hLock h mode = void $ lockImpl h "hLock" mode True

-- | Non-blocking version of 'hLock'.
--
-- Returns 'True' if taking the lock was successful and 'False' otherwise.
--
-- @since base-4.10.0.0
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = lockImpl h "hTryLock" mode False

-- | Release a lock taken with 'hLock' or 'hTryLock'.
--
-- @since base-4.11.0.0
hUnlock :: Handle -> IO ()
hUnlock = unlockImpl

----------------------------------------

