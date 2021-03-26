{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | File locking via the Linux open-fd locking mechanism.
module GHC.IO.Handle.Lock.LinuxOFD where

#include "HsBaseConfig.h"

#if !HAVE_OFD_LOCKING
import GHC.Base () -- Make implicit dependency known to build system
#else

-- Not only is this a good idea but it also works around #17950.
#define _FILE_OFFSET_BITS 64

#include <unistd.h>
#include <fcntl.h>

import Data.Function
import Data.Functor
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Base
import GHC.IO.Exception
import GHC.IO.FD
import GHC.IO.Handle.FD
import GHC.IO.Handle.Lock.Common
import GHC.IO.Handle.Types (Handle)
import GHC.Ptr
import System.Posix.Types (COff, CPid)

-- Linux open file descriptor locking.
--
-- We prefer this over BSD locking (e.g. flock) since the latter appears to
-- break in some NFS configurations. Note that we intentionally do not try to
-- use ordinary POSIX file locking due to its peculiar semantics under
-- multi-threaded environments.

foreign import capi interruptible "fcntl.h fcntl"
  c_fcntl :: CInt -> CInt -> Ptr FLock -> IO CInt

data FLock  = FLock { l_type   :: CShort
                    , l_whence :: CShort
                    , l_start  :: COff
                    , l_len    :: COff
                    , l_pid    :: CPid
                    }

instance Storable FLock where
    sizeOf _ = #{size struct flock}
    alignment _ = #{alignment struct flock}
    poke ptr x = do
        fillBytes ptr 0 (sizeOf x)
        #{poke struct flock, l_type}   ptr (l_type x)
        #{poke struct flock, l_whence} ptr (l_whence x)
        #{poke struct flock, l_start}  ptr (l_start x)
        #{poke struct flock, l_len}    ptr (l_len x)
        #{poke struct flock, l_pid}    ptr (l_pid x)
    peek ptr =
        FLock <$> #{peek struct flock, l_type}   ptr
              <*> #{peek struct flock, l_whence} ptr
              <*> #{peek struct flock, l_start}  ptr
              <*> #{peek struct flock, l_len}    ptr
              <*> #{peek struct flock, l_pid}    ptr

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  with flock $ \flock_ptr -> fix $ \retry -> do
      ret <- c_fcntl fd mode' flock_ptr
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
                  , l_pid = 0
                  }
    mode'
      | block     = #{const F_OFD_SETLKW}
      | otherwise = #{const F_OFD_SETLK}

unlockImpl :: Handle -> IO ()
unlockImpl h = do
  FD{fdFD = fd} <- handleToFd h
  let flock = FLock { l_type = #{const F_UNLCK}
                    , l_whence = #{const SEEK_SET}
                    , l_start = 0
                    , l_len = 0
                    , l_pid = 0
                    }
  throwErrnoIfMinus1_ "hUnlock"
      $ with flock $ c_fcntl fd #{const F_OFD_SETLK}

#endif
