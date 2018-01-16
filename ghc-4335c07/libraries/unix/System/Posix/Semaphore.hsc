#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Semaphore
-- Copyright   :  (c) Daniel Franke 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- POSIX named semaphore support.
--
-----------------------------------------------------------------------------

module System.Posix.Semaphore
    (OpenSemFlags(..), Semaphore(),
     semOpen, semUnlink, semWait, semTryWait, semThreadWait,
     semPost, semGetValue)
    where

#include <semaphore.h>
#include <fcntl.h>

import Foreign.C
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import Control.Concurrent
import Data.Bits

data OpenSemFlags = OpenSemFlags { semCreate :: Bool,
                                   -- ^ If true, create the semaphore if it
                                   --   does not yet exist.
                                   semExclusive :: Bool
                                   -- ^ If true, throw an exception if the
                                   --   semaphore already exists.
                                 }

newtype Semaphore = Semaphore (ForeignPtr ())

-- | Open a named semaphore with the given name, flags, mode, and initial
--   value.
semOpen :: String -> OpenSemFlags -> FileMode -> Int -> IO Semaphore
semOpen name flags mode value =
    let cflags = (if semCreate flags then #{const O_CREAT} else 0) .|.
                 (if semExclusive flags then #{const O_EXCL} else 0)
        semOpen' cname =
            do sem <- throwErrnoPathIfNull "semOpen" name $
                      sem_open cname (toEnum cflags) mode (toEnum value)
               fptr <- newForeignPtr sem (finalize sem)
               return $ Semaphore fptr
        finalize sem = throwErrnoPathIfMinus1_ "semOpen" name $
                       sem_close sem in
    withCAString name semOpen'

-- | Delete the semaphore with the given name.
semUnlink :: String -> IO ()
semUnlink name = withCAString name semUnlink'
    where semUnlink' cname = throwErrnoPathIfMinus1_ "semUnlink" name $
                             sem_unlink cname

-- | Lock the semaphore, blocking until it becomes available.  Since this
--   is done through a system call, this will block the *entire runtime*,
--   not just the current thread.  If this is not the behaviour you want,
--   use semThreadWait instead.
semWait :: Semaphore -> IO ()
semWait (Semaphore fptr) = withForeignPtr fptr semWait'
    where semWait' sem = throwErrnoIfMinus1Retry_ "semWait" $
                         sem_wait sem

-- | Attempt to lock the semaphore without blocking.  Immediately return
--   False if it is not available.
semTryWait :: Semaphore -> IO Bool
semTryWait (Semaphore fptr) = withForeignPtr fptr semTrywait'
    where semTrywait' sem = do res <- sem_trywait sem
                               (if res == 0 then return True
                                else do errno <- getErrno
                                        (if errno == eINTR
                                         then semTrywait' sem
                                         else if errno == eAGAIN
                                              then return False
                                              else throwErrno "semTrywait"))

-- | Poll the semaphore until it is available, then lock it.  Unlike
--   semWait, this will block only the current thread rather than the
--   entire process.
semThreadWait :: Semaphore -> IO ()
semThreadWait sem = do res <- semTryWait sem
                       (if res then return ()
                        else ( do { yield; semThreadWait sem } ))

-- | Unlock the semaphore.
semPost :: Semaphore -> IO ()
semPost (Semaphore fptr) = withForeignPtr fptr semPost'
    where semPost' sem = throwErrnoIfMinus1Retry_ "semPost" $
                         sem_post sem

-- | Return the semaphore's current value.
semGetValue :: Semaphore -> IO Int
semGetValue (Semaphore fptr) = withForeignPtr fptr semGetValue'
    where semGetValue' sem = alloca (semGetValue_ sem)

semGetValue_ :: Ptr () -> Ptr CInt -> IO Int
semGetValue_ sem ptr = do throwErrnoIfMinus1Retry_ "semGetValue" $
                            sem_getvalue sem ptr
                          cint <- peek ptr
                          return $ fromEnum cint

foreign import ccall safe "sem_open"
        sem_open :: CString -> CInt -> CMode -> CUInt -> IO (Ptr ())
foreign import ccall safe "sem_close"
        sem_close :: Ptr () -> IO CInt
foreign import ccall safe "sem_unlink"
        sem_unlink :: CString -> IO CInt

foreign import ccall safe "sem_wait"
        sem_wait :: Ptr () -> IO CInt
foreign import ccall safe "sem_trywait"
        sem_trywait :: Ptr () -> IO CInt
foreign import ccall safe "sem_post"
        sem_post :: Ptr () -> IO CInt
foreign import ccall safe "sem_getvalue"
        sem_getvalue :: Ptr () -> Ptr CInt -> IO Int
