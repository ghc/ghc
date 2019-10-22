{-# LANGUAGE NoImplicitPrelude #-}

-- | Things common to all file locking implementations.
module GHC.IO.Handle.Lock.Common
  ( FileLockingNotSupported(..)
  , LockMode(..)
  ) where

import GHC.Exception
import GHC.Show

-- | Exception thrown by 'hLock' on non-Windows platforms that don't support
-- 'flock'.
data FileLockingNotSupported = FileLockingNotSupported
  deriving Show -- ^ @since 4.10.0.0

-- ^ @since 4.10.0.0
instance Exception FileLockingNotSupported

-- | Indicates a mode in which a file should be locked.
data LockMode = SharedLock | ExclusiveLock
