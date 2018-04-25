{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI, RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Process.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX process support.  See also the System.Cmd and System.Process
-- modules in the process package.
--
-----------------------------------------------------------------------------

module System.Posix.Process.Common (
    -- * Processes

    -- ** Forking and executing
    forkProcess,
    forkProcessWithUnmask,

    -- ** Exiting
    exitImmediately,

    -- ** Process environment
    getProcessID,
    getParentProcessID,

    -- ** Process groups
    getProcessGroupID,
    getProcessGroupIDOf,
    createProcessGroupFor,
    joinProcessGroup,
    setProcessGroupIDOf,

    -- ** Sessions
    createSession,

    -- ** Process times
    ProcessTimes(..),
    getProcessTimes,

    -- ** Scheduling priority
    nice,
    getProcessPriority,
    getProcessGroupPriority,
    getUserPriority,
    setProcessPriority,
    setProcessGroupPriority,
    setUserPriority,

    -- ** Process status
    ProcessStatus(..),
    getProcessStatus,
    getAnyProcessStatus,
    getGroupProcessStatus,

    -- ** Deprecated
    createProcessGroup,
    setProcessGroupID,

 ) where

#include "HsUnix.h"

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Ptr ( Ptr )
import Foreign.StablePtr ( StablePtr, newStablePtr, freeStablePtr )
import Foreign.Storable ( Storable(..) )
import System.Exit
import System.Posix.Process.Internals
import System.Posix.Types
import Control.Monad

import Control.Exception.Base ( bracket, getMaskingState, MaskingState(..) ) -- used by forkProcess
import GHC.TopHandler   ( runIO )
import GHC.IO ( unsafeUnmask, uninterruptibleMask_ )

-- -----------------------------------------------------------------------------
-- Process environment

-- | 'getProcessID' calls @getpid@ to obtain the 'ProcessID' for
--   the current process.
getProcessID :: IO ProcessID
getProcessID = c_getpid

foreign import ccall unsafe "getpid"
   c_getpid :: IO CPid

-- | 'getProcessID' calls @getppid@ to obtain the 'ProcessID' for
--   the parent of the current process.
getParentProcessID :: IO ProcessID
getParentProcessID = c_getppid

foreign import ccall unsafe "getppid"
  c_getppid :: IO CPid

-- | 'getProcessGroupID' calls @getpgrp@ to obtain the
--   'ProcessGroupID' for the current process.
getProcessGroupID :: IO ProcessGroupID
getProcessGroupID = c_getpgrp

foreign import ccall unsafe "getpgrp"
  c_getpgrp :: IO CPid

-- | @'getProcessGroupIDOf' pid@ calls @getpgid@ to obtain the
--   'ProcessGroupID' for process @pid@.
getProcessGroupIDOf :: ProcessID -> IO ProcessGroupID
getProcessGroupIDOf pid =
  throwErrnoIfMinus1 "getProcessGroupIDOf" (c_getpgid pid)

foreign import ccall unsafe "getpgid"
  c_getpgid :: CPid -> IO CPid

{-
   To be added in the future, after the deprecation period for the
   existing createProcessGroup has elapsed:

-- | 'createProcessGroup' calls @setpgid(0,0)@ to make
--   the current process a new process group leader.
createProcessGroup :: IO ProcessGroupID
createProcessGroup = do
  throwErrnoIfMinus1_ "createProcessGroup" (c_setpgid 0 0)
  pgid <- getProcessGroupID
  return pgid
-}

-- | @'createProcessGroupFor' pid@ calls @setpgid@ to make
--   process @pid@ a new process group leader.
createProcessGroupFor :: ProcessID -> IO ProcessGroupID
createProcessGroupFor pid = do
  throwErrnoIfMinus1_ "createProcessGroupFor" (c_setpgid pid 0)
  return pid

-- | @'joinProcessGroup' pgid@ calls @setpgid@ to set the
--   'ProcessGroupID' of the current process to @pgid@.
joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid =
  throwErrnoIfMinus1_ "joinProcessGroup" (c_setpgid 0 pgid)

{-
   To be added in the future, after the deprecation period for the
   existing setProcessGroupID has elapsed:

-- | @'setProcessGroupID' pgid@ calls @setpgid@ to set the
--   'ProcessGroupID' of the current process to @pgid@.
setProcessGroupID :: ProcessGroupID -> IO ()
setProcessGroupID pgid =
  throwErrnoIfMinus1_ "setProcessGroupID" (c_setpgid 0 pgid)
-}

-- | @'setProcessGroupIDOf' pid pgid@ calls @setpgid@ to set the
--   'ProcessGroupIDOf' for process @pid@ to @pgid@.
setProcessGroupIDOf :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupIDOf pid pgid =
  throwErrnoIfMinus1_ "setProcessGroupIDOf" (c_setpgid pid pgid)

foreign import ccall unsafe "setpgid"
  c_setpgid :: CPid -> CPid -> IO CInt

-- | 'createSession' calls @setsid@ to create a new session
--   with the current process as session leader.
createSession :: IO ProcessGroupID
createSession = throwErrnoIfMinus1 "createSession" c_setsid

foreign import ccall unsafe "setsid"
  c_setsid :: IO CPid

-- -----------------------------------------------------------------------------
-- Process times

-- All times in clock ticks (see getClockTick)

data ProcessTimes
  = ProcessTimes { elapsedTime     :: ClockTick
                 , userTime        :: ClockTick
                 , systemTime      :: ClockTick
                 , childUserTime   :: ClockTick
                 , childSystemTime :: ClockTick
                 }

-- | 'getProcessTimes' calls @times@ to obtain time-accounting
--   information for the current process and its children.
getProcessTimes :: IO ProcessTimes
getProcessTimes = do
   allocaBytes (#const sizeof(struct tms)) $ \p_tms -> do
     elapsed <- throwErrnoIfMinus1 "getProcessTimes" (c_times p_tms)
     ut  <- (#peek struct tms, tms_utime)  p_tms
     st  <- (#peek struct tms, tms_stime)  p_tms
     cut <- (#peek struct tms, tms_cutime) p_tms
     cst <- (#peek struct tms, tms_cstime) p_tms
     return (ProcessTimes{ elapsedTime     = elapsed,
                           userTime        = ut,
                           systemTime      = st,
                           childUserTime   = cut,
                           childSystemTime = cst
                          })

data {-# CTYPE "struct tms" #-} CTms

foreign import capi unsafe "HsUnix.h times"
  c_times :: Ptr CTms -> IO CClock

-- -----------------------------------------------------------------------------
-- Process scheduling priority

nice :: Int -> IO ()
nice prio = do
  resetErrno
  res <- c_nice (fromIntegral prio)
  when (res == -1) $ do
    err <- getErrno
    when (err /= eOK) (throwErrno "nice")

foreign import ccall unsafe "nice"
  c_nice :: CInt -> IO CInt

getProcessPriority      :: ProcessID      -> IO Int
getProcessGroupPriority :: ProcessGroupID -> IO Int
getUserPriority         :: UserID         -> IO Int

getProcessPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (#const PRIO_PROCESS) (fromIntegral pid)
  return (fromIntegral r)

getProcessGroupPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (#const PRIO_PGRP) (fromIntegral pid)
  return (fromIntegral r)

getUserPriority uid = do
  r <- throwErrnoIfMinus1 "getUserPriority" $
         c_getpriority (#const PRIO_USER) (fromIntegral uid)
  return (fromIntegral r)

foreign import ccall unsafe "getpriority"
  c_getpriority :: CInt -> CInt -> IO CInt

setProcessPriority      :: ProcessID      -> Int -> IO ()
setProcessGroupPriority :: ProcessGroupID -> Int -> IO ()
setUserPriority         :: UserID         -> Int -> IO ()

setProcessPriority pid val =
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (#const PRIO_PROCESS) (fromIntegral pid) (fromIntegral val)

setProcessGroupPriority pid val =
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (#const PRIO_PGRP) (fromIntegral pid) (fromIntegral val)

setUserPriority uid val =
  throwErrnoIfMinus1_ "setUserPriority" $
    c_setpriority (#const PRIO_USER) (fromIntegral uid) (fromIntegral val)

foreign import ccall unsafe "setpriority"
  c_setpriority :: CInt -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Forking, execution

{- | 'forkProcess' corresponds to the POSIX @fork@ system call.
The 'IO' action passed as an argument is executed in the child process; no other
threads will be copied to the child process.
On success, 'forkProcess' returns the child's 'ProcessID' to the parent process;
in case of an error, an exception is thrown.

The exception masking state of the executed action is inherited
(c.f. 'forkIO'), see also 'forkProcessWithUnmask' (/since: 2.7.0.0/).

'forkProcess' comes with a giant warning: since any other running
threads are not copied into the child process, it's easy to go wrong:
e.g. by accessing some shared resource that was held by another thread
in the parent.
-}

forkProcess :: IO () -> IO ProcessID
forkProcess action = do
  -- wrap action to re-establish caller's masking state, as
  -- 'forkProcessPrim' starts in 'MaskedInterruptible' state by
  -- default; see also #1048
  mstate <- getMaskingState
  let action' = case mstate of
          Unmasked              -> unsafeUnmask action
          MaskedInterruptible   -> action
          MaskedUninterruptible -> uninterruptibleMask_ action

  bracket
    (newStablePtr (runIO action'))
    freeStablePtr
    (\stable -> throwErrnoIfMinus1 "forkProcess" (forkProcessPrim stable))

foreign import ccall "forkProcess" forkProcessPrim :: StablePtr (IO ()) -> IO CPid

-- | Variant of 'forkProcess' in the style of 'forkIOWithUnmask'.
--
-- @since 2.7.0.0
forkProcessWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ProcessID
forkProcessWithUnmask action = forkProcess (action unsafeUnmask)

-- -----------------------------------------------------------------------------
-- Waiting for process termination

-- | @'getProcessStatus' blk stopped pid@ calls @waitpid@, returning
--   @'Just' tc@, the 'ProcessStatus' for process @pid@ if it is
--   available, 'Nothing' otherwise.  If @blk@ is 'False', then
--   @WNOHANG@ is set in the options for @waitpid@, otherwise not.
--   If @stopped@ is 'True', then @WUNTRACED@ is set in the
--   options for @waitpid@, otherwise not.
getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid =
  alloca $ \wstatp -> do
    pid' <- throwErrnoIfMinus1Retry "getProcessStatus"
                (c_waitpid pid wstatp (waitOptions block stopped))
    case pid' of
      0  -> return Nothing
      _  -> do ps <- readWaitStatus wstatp
               return (Just ps)

-- safe/interruptible, because this call might block
foreign import ccall interruptible "waitpid"
  c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

-- | @'getGroupProcessStatus' blk stopped pgid@ calls @waitpid@,
--   returning @'Just' (pid, tc)@, the 'ProcessID' and 'ProcessStatus'
--   for any process in group @pgid@ if one is available, or 'Nothing'
--   if there are child processes but none have exited.  If there are
--   no child processes, then 'getGroupProcessStatus' raises an
--   'isDoesNotExistError' exception.
--
--   If @blk@ is 'False', then @WNOHANG@ is set in the options for
--   @waitpid@, otherwise not.  If @stopped@ is 'True', then
--   @WUNTRACED@ is set in the options for @waitpid@, otherwise not.
getGroupProcessStatus :: Bool
                      -> Bool
                      -> ProcessGroupID
                      -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid =
  alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getGroupProcessStatus"
                (c_waitpid (-pgid) wstatp (waitOptions block stopped))
    case pid of
      0  -> return Nothing
      _  -> do ps <- readWaitStatus wstatp
               return (Just (pid, ps))

-- | @'getAnyProcessStatus' blk stopped@ calls @waitpid@, returning
--   @'Just' (pid, tc)@, the 'ProcessID' and 'ProcessStatus' for any
--   child process if a child process has exited, or 'Nothing' if
--   there are child processes but none have exited.  If there are no
--   child processes, then 'getAnyProcessStatus' raises an
--   'isDoesNotExistError' exception.
--
--   If @blk@ is 'False', then @WNOHANG@ is set in the options for
--   @waitpid@, otherwise not.  If @stopped@ is 'True', then
--   @WUNTRACED@ is set in the options for @waitpid@, otherwise not.
getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped = getGroupProcessStatus block stopped 1

waitOptions :: Bool -> Bool -> CInt
--             block   stopped
waitOptions False False = (#const WNOHANG)
waitOptions False True  = (#const (WNOHANG|WUNTRACED))
waitOptions True  False = 0
waitOptions True  True  = (#const WUNTRACED)

-- Turn a (ptr to a) wait status into a ProcessStatus

readWaitStatus :: Ptr CInt -> IO ProcessStatus
readWaitStatus wstatp = do
  wstat <- peek wstatp
  decipherWaitStatus wstat

-- -----------------------------------------------------------------------------
-- Exiting

-- | @'exitImmediately' status@ calls @_exit@ to terminate the process
--   with the indicated exit @status@.
--   The operation never returns.
exitImmediately :: ExitCode -> IO ()
exitImmediately exitcode = c_exit (exitcode2Int exitcode)
  where
    exitcode2Int ExitSuccess = 0
    exitcode2Int (ExitFailure n) = fromIntegral n

foreign import ccall unsafe "exit"
  c_exit :: CInt -> IO ()

-- -----------------------------------------------------------------------------
-- Deprecated or subject to change

{-# DEPRECATED createProcessGroup "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use 'createProcessGroupFor' instead." #-} -- deprecated in 7.2
-- | @'createProcessGroup' pid@ calls @setpgid@ to make
--   process @pid@ a new process group leader.
--   This function is currently deprecated,
--   and might be changed to making the current
--   process a new process group leader in future versions.
createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = do
  throwErrnoIfMinus1_ "createProcessGroup" (c_setpgid pid 0)
  return pid

{-# DEPRECATED setProcessGroupID "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use 'setProcessGroupIDOf' instead." #-} -- deprecated in 7.2
-- | @'setProcessGroupID' pid pgid@ calls @setpgid@ to set the
--   'ProcessGroupID' for process @pid@ to @pgid@.
--   This function is currently deprecated,
--   and might be changed to setting the 'ProcessGroupID'
--   for the current process in future versions.
setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid =
  throwErrnoIfMinus1_ "setProcessGroupID" (c_setpgid pid pgid)

-- -----------------------------------------------------------------------------
