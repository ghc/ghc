{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Unistd
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX miscellaneous stuff, mostly from unistd.h
--
-----------------------------------------------------------------------------

module System.Posix.Unistd (
    -- * System environment
    SystemID(..),
    getSystemID,

    SysVar(..),
    getSysVar,

    -- * Sleeping
    sleep, usleep, nanosleep,

    -- * File synchronisation
    fileSynchronise,
    fileSynchroniseDataOnly,

  {-
    ToDo from unistd.h:
      confstr,
      lots of sysconf variables

    -- use Network.BSD
    gethostid, gethostname

    -- should be in System.Posix.Files?
    pathconf, fpathconf,

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where

#include "HsUnix.h"

import Foreign.C.Error
import Foreign.C.String ( peekCString )
import Foreign.C.Types
import Foreign
import System.Posix.Types
import System.Posix.Internals

#if !(HAVE_FSYNC && HAVE_FDATASYNC)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

-- -----------------------------------------------------------------------------
-- System environment (uname())

data SystemID =
  SystemID { systemName :: String
           , nodeName   :: String
           , release    :: String
           , version    :: String
           , machine    :: String
           }

getSystemID :: IO SystemID
getSystemID = do
  allocaBytes (#const sizeof(struct utsname)) $ \p_sid -> do
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString ((#ptr struct utsname, sysname) p_sid)
    node <- peekCString ((#ptr struct utsname, nodename) p_sid)
    rel  <- peekCString ((#ptr struct utsname, release) p_sid)
    ver  <- peekCString ((#ptr struct utsname, version) p_sid)
    mach <- peekCString ((#ptr struct utsname, machine) p_sid)
    return (SystemID { systemName = sysN,
                       nodeName   = node,
                       release    = rel,
                       version    = ver,
                       machine    = mach
                     })

foreign import ccall unsafe "uname"
   c_uname :: Ptr CUtsname -> IO CInt

-- -----------------------------------------------------------------------------
-- sleeping

-- | Sleep for the specified duration (in seconds).  Returns the time remaining
-- (if the sleep was interrupted by a signal, for example).
--
-- /GHC Note/: 'Control.Concurrent.threadDelay' is a better choice.  Since GHC
-- uses signals for its internal clock, a call to 'sleep' will usually be
-- interrupted immediately.  That makes 'sleep' unusable in a program compiled
-- with GHC, unless the RTS timer is disabled (with @+RTS -V0@).  Furthermore,
-- without the @-threaded@ option, 'sleep' will block all other user threads.
-- Even with the @-threaded@ option, 'sleep' requires a full OS thread to
-- itself.  'Control.Concurrent.threadDelay' has none of these shortcomings.
--
sleep :: Int -> IO Int
sleep 0 = return 0
sleep secs = do r <- c_sleep (fromIntegral secs); return (fromIntegral r)

{-# WARNING sleep "This function has several shortcomings (see documentation). Please consider using Control.Concurrent.threadDelay instead." #-}

foreign import ccall safe "sleep"
  c_sleep :: CUInt -> IO CUInt

-- | Sleep for the specified duration (in microseconds).
--
-- /GHC Note/: 'Control.Concurrent.threadDelay' is a better choice.
-- Without the @-threaded@ option, 'usleep' will block all other user
-- threads.  Even with the @-threaded@ option, 'usleep' requires a
-- full OS thread to itself.  'Control.Concurrent.threadDelay' has
-- neither of these shortcomings.
--
usleep :: Int -> IO ()
#ifdef HAVE_NANOSLEEP
usleep usecs = nanosleep (fromIntegral usecs * 1000)
#else
usleep 0 = return ()
#ifdef USLEEP_RETURNS_VOID
usleep usecs = c_usleep (fromIntegral usecs)
#else
usleep usecs = throwErrnoIfMinus1_ "usleep" (c_usleep (fromIntegral usecs))
#endif

#ifdef USLEEP_RETURNS_VOID
foreign import ccall safe "usleep"
  c_usleep :: CUInt -> IO ()
#else
foreign import ccall safe "usleep"
  c_usleep :: CUInt -> IO CInt
#endif
#endif /* HAVE_NANOSLEEP */

-- | Sleep for the specified duration (in nanoseconds)
--
-- /GHC Note/: the comment for 'usleep' also applies here.
nanosleep :: Integer -> IO ()
#ifndef HAVE_NANOSLEEP
nanosleep = error "nanosleep: not available on this platform"
#else
nanosleep 0 = return ()
nanosleep nsecs = do
  allocaBytes (#const sizeof(struct timespec)) $ \pts1 -> do
  allocaBytes (#const sizeof(struct timespec)) $ \pts2 -> do
     let (tv_sec0, tv_nsec0) = nsecs `divMod` 1000000000
     let
       loop tv_sec tv_nsec = do
         (#poke struct timespec, tv_sec)  pts1 tv_sec
         (#poke struct timespec, tv_nsec) pts1 tv_nsec
         res <- c_nanosleep pts1 pts2
         if res == 0
            then return ()
            else do errno <- getErrno
                    if errno == eINTR
                       then do
                           tv_sec'  <- (#peek struct timespec, tv_sec)  pts2
                           tv_nsec' <- (#peek struct timespec, tv_nsec) pts2
                           loop tv_sec' tv_nsec'
                       else throwErrno "nanosleep"
     loop (fromIntegral tv_sec0 :: CTime) (fromIntegral tv_nsec0 :: CTime)

data {-# CTYPE "struct timespec" #-} CTimeSpec

foreign import capi safe "HsUnix.h nanosleep"
  c_nanosleep :: Ptr CTimeSpec -> Ptr CTimeSpec -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- System variables

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl
        -- ToDo: lots more

getSysVar :: SysVar -> IO Integer
getSysVar v =
    case v of
      ArgumentLimit -> sysconf (#const _SC_ARG_MAX)
      ChildLimit    -> sysconf (#const _SC_CHILD_MAX)
      ClockTick     -> sysconf (#const _SC_CLK_TCK)
      GroupLimit    -> sysconf (#const _SC_NGROUPS_MAX)
      OpenFileLimit -> sysconf (#const _SC_OPEN_MAX)
      PosixVersion  -> sysconf (#const _SC_VERSION)
      HasSavedIDs   -> sysconf (#const _SC_SAVED_IDS)
      HasJobControl -> sysconf (#const _SC_JOB_CONTROL)

sysconf :: CInt -> IO Integer
sysconf n = do
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong

-- -----------------------------------------------------------------------------
-- File synchronization

-- | Performs @fsync(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @fsync(2)@ (use @#if HAVE_FSYNC@ CPP guard to
-- detect availability).
--
-- @since 2.7.1.0
fileSynchronise :: Fd -> IO ()
#if HAVE_FSYNC
fileSynchronise fd = do
  throwErrnoIfMinus1_ "fileSynchronise" (c_fsync fd)

foreign import capi safe "unistd.h fsync"
  c_fsync :: Fd -> IO CInt
#else
{-# WARNING fileSynchronise
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_FSYNC@)" #-}
fileSynchronise _ = ioError (ioeSetLocation unsupportedOperation
                             "fileSynchronise")
#endif

-- | Performs @fdatasync(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @fdatasync(2)@ (use @#if HAVE_FDATASYNC@ CPP guard to
-- detect availability).
--
-- @since 2.7.1.0
fileSynchroniseDataOnly :: Fd -> IO ()
#if HAVE_FDATASYNC
fileSynchroniseDataOnly fd = do
  throwErrnoIfMinus1_ "fileSynchroniseDataOnly" (c_fdatasync fd)

foreign import capi safe "unistd.h fdatasync"
  c_fdatasync :: Fd -> IO CInt
#else
{-# WARNING fileSynchroniseDataOnly
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_FDATASYNC@)" #-}
fileSynchroniseDataOnly _ = ioError (ioeSetLocation unsupportedOperation
                                     "fileSynchroniseDataOnly")
#endif
