{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | File locking via POSIX @flock@.
module GHC.Internal.IO.Handle.Lock.Flock where

#include "HsBaseConfig.h"

#if !HAVE_FLOCK
-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Types ()
#else

#include <sys/file.h>

import GHC.Internal.Data.Bits
import GHC.Internal.Data.Function
import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Base
import GHC.Internal.IO.Exception
import GHC.Internal.IO.FD
import GHC.Internal.IO.Handle.FD
import GHC.Internal.IO.Handle.Lock.Common
import GHC.Internal.IO.Handle.Types (Handle)

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

#endif
