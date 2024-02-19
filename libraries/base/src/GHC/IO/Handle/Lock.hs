module GHC.IO.Handle.Lock
    (FileLockingNotSupported(..),
     LockMode(..),
     hLock,
     hTryLock,
     hUnlock
     ) where

import GHC.Internal.IO.Handle.Lock