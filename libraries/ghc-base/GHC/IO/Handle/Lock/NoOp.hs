{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Handle.Lock.NoOp where

import GHC.Base
import GHC.IO (throwIO)
import GHC.IO.Handle.Lock.Common
import GHC.IO.Handle.Types (Handle)

-- | No-op implementation.
lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl _ _ _ _ = throwIO FileLockingNotSupported

-- | No-op implementation.
unlockImpl :: Handle -> IO ()
unlockImpl _ = throwIO FileLockingNotSupported
