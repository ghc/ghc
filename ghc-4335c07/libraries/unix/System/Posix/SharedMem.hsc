#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.SharedMem
-- Copyright   :  (c) Daniel Franke 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- POSIX shared memory support.
--
-----------------------------------------------------------------------------

module System.Posix.SharedMem
    (ShmOpenFlags(..), shmOpen, shmUnlink)
    where

#include "HsUnix.h"

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

import System.Posix.Types
#if defined(HAVE_SHM_OPEN) || defined(HAVE_SHM_UNLINK)
import Foreign.C
#endif
#ifdef HAVE_SHM_OPEN
import Data.Bits
#endif

data ShmOpenFlags = ShmOpenFlags
    { shmReadWrite :: Bool,
      -- ^ If true, open the shm object read-write rather than read-only.
      shmCreate :: Bool,
      -- ^ If true, create the shm object if it does not exist.
      shmExclusive :: Bool,
      -- ^ If true, throw an exception if the shm object already exists.
      shmTrunc :: Bool
      -- ^ If true, wipe the contents of the shm object after opening it.
    }

-- | Open a shared memory object with the given name, flags, and mode.
shmOpen :: String -> ShmOpenFlags -> FileMode -> IO Fd
#ifdef HAVE_SHM_OPEN
shmOpen name flags mode =
    do cflags0 <- return 0
       cflags1 <- return $ cflags0 .|. (if shmReadWrite flags
                                        then #{const O_RDWR}
                                        else #{const O_RDONLY})
       cflags2 <- return $ cflags1 .|. (if shmCreate flags then #{const O_CREAT}
                                        else 0)
       cflags3 <- return $ cflags2 .|. (if shmExclusive flags
                                        then #{const O_EXCL}
                                        else 0)
       cflags4 <- return $ cflags3 .|. (if shmTrunc flags then #{const O_TRUNC}
                                        else 0)
       withCAString name (shmOpen' cflags4)
    where shmOpen' cflags cname =
              do fd <- throwErrnoIfMinus1 "shmOpen" $
                       shm_open cname cflags mode
                 return $ Fd fd
#else
shmOpen = error "System.Posix.SharedMem:shm_open: not available"
#endif

-- | Delete the shared memory object with the given name.
shmUnlink :: String -> IO ()
#ifdef HAVE_SHM_UNLINK
shmUnlink name = withCAString name shmUnlink'
    where shmUnlink' cname =
              throwErrnoIfMinus1_ "shmUnlink" $ shm_unlink cname
#else
shmUnlink = error "System.Posix.SharedMem:shm_unlink: not available"
#endif

#ifdef HAVE_SHM_OPEN
foreign import ccall unsafe "shm_open"
        shm_open :: CString -> CInt -> CMode -> IO CInt
#endif

#ifdef HAVE_SHM_UNLINK
foreign import ccall unsafe "shm_unlink"
        shm_unlink :: CString -> IO CInt
#endif
