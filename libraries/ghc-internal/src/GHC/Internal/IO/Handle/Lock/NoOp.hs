{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle.Lock.NoOp where

import GHC.Internal.Base
import GHC.Internal.IO (throwIO)
import GHC.Internal.IO.Handle.Lock.Common
import GHC.Internal.IO.Handle.Types (Handle)

-- | No-op implementation.
lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl _ _ _ _ = throwIO FileLockingNotSupported

-- | No-op implementation.
unlockImpl :: Handle -> IO ()
unlockImpl _ = throwIO FileLockingNotSupported
