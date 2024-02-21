{-# LANGUAGE NoImplicitPrelude #-}

-- | Things common to all file locking implementations.
module GHC.Internal.IO.Handle.Lock.Common
  ( FileLockingNotSupported(..)
  , LockMode(..)
  ) where

import GHC.Internal.Exception
import GHC.Internal.Show

-- | Exception thrown by 'hLock' on non-Windows platforms that don't support
-- 'flock'.
data FileLockingNotSupported = FileLockingNotSupported
  deriving Show -- ^ @since base-4.10.0.0

-- ^ @since base-4.10.0.0
instance Exception FileLockingNotSupported

-- | Indicates a mode in which a file should be locked.
data LockMode = SharedLock | ExclusiveLock
