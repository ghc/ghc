%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixProcEnv]{Haskell 1.3 POSIX Process Environment}

\begin{code}

#include "config.h"

module PosixProcEnv (
    ProcessTimes,
    SysVar(..),
    SystemID,
    childSystemTime,
    childUserTime,
    createProcessGroup,
    createSession,
    elapsedTime,
    epochTime,
#if !defined(cygwin32_TARGET_OS)
    getControllingTerminalName,
#endif
    getEffectiveGroupID,
    getEffectiveUserID,
    getEffectiveUserName,
#if !defined(cygwin32_TARGET_OS)
    getGroups,
#endif
    getLoginName,
    getParentProcessID,
    getProcessGroupID,
    getProcessID,
    getProcessTimes,
    getRealGroupID,
    getRealUserID,
    getSysVar,
    getSystemID,
    getTerminalName,
    joinProcessGroup,
    machine,
    nodeName,
    queryTerminal,
    release,
    setGroupID,
    setProcessGroupID,
    setUserID,
    systemName,
    systemTime,
    userTime,
    version
    ) where

import GlaExts
import PrelArr (ByteArray(..)) -- see internals
import PrelIOBase
import IO
import Addr	( nullAddr )

import PosixErr
import PosixUtil
import CString   ( strcpy, allocWords, freeze, allocChars )

\end{code}

\begin{code}
getProcessID :: IO ProcessID
getProcessID = _ccall_ getpid

getParentProcessID :: IO ProcessID
getParentProcessID = _ccall_ getppid

getRealUserID :: IO UserID
getRealUserID = _ccall_ getuid

getEffectiveUserID :: IO UserID
getEffectiveUserID = _ccall_ geteuid

setUserID :: UserID -> IO ()
setUserID uid = nonzero_error (_ccall_ setuid uid) "setUserID"

getLoginName :: IO String
getLoginName =  do
    str <- _ccall_ getlogin
    if str == nullAddr
       then syserr "getLoginName"
       else strcpy str

getRealGroupID :: IO GroupID
getRealGroupID = _ccall_ getgid

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = _ccall_ getegid

setGroupID :: GroupID -> IO ()
setGroupID gid = nonzero_error (_ccall_ setgid gid) "setGroupID"

-- getgroups() is not supported in beta18 of
-- cygwin32
#if !defined(cygwin32_TARGET_OS)
getGroups :: IO [GroupID]
getGroups = do
    ngroups <- _ccall_ getgroups (0::Int) nullAddr
    words   <- allocWords ngroups
    ngroups <- _casm_ ``%r = getgroups(%0,(gid_t *)%1);'' ngroups words
    if ngroups /= ((-1)::Int)
       then do
	 arr <- freeze words
         return (map (extract arr) [0..(ngroups-1)])
       else
	 syserr "getGroups"
  where
    extract (ByteArray _ barr#) (I# n#) =
        case indexIntArray# barr# n# of
	  r# -> (I# r#)
#endif

getEffectiveUserName :: IO String
getEffectiveUserName = do
 {- cuserid() is deprecated, using getpwuid() instead. -}
    euid <- getEffectiveUserID
    ptr  <- _ccall_ getpwuid euid
    str  <- _casm_ ``%r = ((struct passwd *)%0)->pw_name;'' (ptr::Addr)
    strcpy str   

{- OLD:
    str <- _ccall_ cuserid nullAddr
    if str == nullAddr
       then syserr "getEffectiveUserName"
       else strcpy str
-}

getProcessGroupID :: IO ProcessGroupID
getProcessGroupID = _ccall_ getpgrp

createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = do
    pgid <- _ccall_ setpgid pid (0::Int)
    if pgid == (0::Int)
       then return pgid
       else syserr "createProcessGroup"

joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid =
    nonzero_error (_ccall_ setpgid (0::Int) pgid) "joinProcessGroupID"

setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid =
    nonzero_error (_ccall_ setpgid pid pgid) "setProcessGroupID"

createSession :: IO ProcessGroupID
createSession = do
    pgid <- _ccall_ setsid
    if pgid /= ((-1)::Int)
       then return pgid
       else syserr "createSession"

type SystemID = ByteArray Int

systemName :: SystemID -> String
systemName sid =  unsafePerformIO $ do
    str <-_casm_ ``%r = ((struct utsname *)%0)->sysname;'' sid
    strcpy str

nodeName :: SystemID -> String
nodeName sid =  unsafePerformIO $ do
    str <- _casm_ ``%r = ((struct utsname *)%0)->nodename;'' sid
    strcpy str

release :: SystemID -> String
release sid =  unsafePerformIO $ do
    str <- _casm_ ``%r = ((struct utsname *)%0)->release;'' sid
    strcpy str

version :: SystemID -> String
version sid =  unsafePerformIO $ do
    str <- _casm_ ``%r = ((struct utsname *)%0)->version;'' sid
    strcpy str

machine :: SystemID -> String
machine sid = unsafePerformIO $ do
    str <- _casm_ ``%r = ((struct utsname *)%0)->machine;'' sid
    strcpy str

getSystemID :: IO SystemID
getSystemID = do
    bytes <- allocChars (``sizeof(struct utsname)''::Int)
    rc    <- _casm_ ``%r = uname((struct utsname *)%0);'' bytes
    if rc /= ((-1)::Int)
       then freeze bytes
       else syserr "getSystemID"

epochTime :: IO EpochTime
epochTime = do
    secs <- _ccall_ time nullAddr
    if secs /= ((-1)::Int)
       then return secs
       else syserr "epochTime"

-- All times in clock ticks (see getClockTick)

type ProcessTimes = (ClockTick, ByteArray Int)

elapsedTime :: ProcessTimes -> ClockTick
elapsedTime (realtime, _) = realtime

userTime :: ProcessTimes -> ClockTick
userTime (_, times) = unsafePerformIO $
    _casm_ ``%r = ((struct tms *)%0)->tms_utime;'' times

systemTime :: ProcessTimes -> ClockTick
systemTime (_, times) = unsafePerformIO $
    _casm_ ``%r = ((struct tms *)%0)->tms_stime;'' times

childUserTime :: ProcessTimes -> ClockTick
childUserTime (_, times) = unsafePerformIO $
    _casm_ ``%r = ((struct tms *)%0)->tms_cutime;'' times

childSystemTime :: ProcessTimes -> ClockTick
childSystemTime (_, times) = unsafePerformIO $
    _casm_ ``%r = ((struct tms *)%0)->tms_cstime;'' times

getProcessTimes :: IO ProcessTimes
getProcessTimes = do
    bytes <- allocChars (``sizeof(struct tms)''::Int)
    elapsed <- _casm_ ``%r = times((struct tms *)%0);'' bytes
    if elapsed /= ((-1)::Int)
       then do
	    times <- freeze bytes
	    return (elapsed, times)
       else
	    syserr "getProcessTimes"

#if !defined(cygwin32_TARGET_OS)
getControllingTerminalName :: IO FilePath
getControllingTerminalName = do
    str <- _ccall_ ctermid nullAddr
    if str == nullAddr
       then ioError (IOError Nothing NoSuchThing "getControllingTerminalName" "no controlling terminal")
       else strcpy str
#endif

getTerminalName :: Fd -> IO FilePath
getTerminalName fd = do
    str <- _ccall_ ttyname fd
    if str == nullAddr
       then do
        err <- try (queryTerminal fd)
        either (\ _err -> syserr "getTerminalName")
               (\ succ -> if succ then ioError (IOError Nothing NoSuchThing
						"getTerminalName" "no name")
                          else ioError (IOError Nothing InappropriateType
						"getTerminalName" "not a terminal"))
           err
       else strcpy str

queryTerminal :: Fd -> IO Bool
queryTerminal (FD# fd) = do
    rc <- _ccall_ isatty fd
    case (rc::Int) of
      -1 -> syserr "queryTerminal"
      0  -> return False
      1  -> return True

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl

getSysVar :: SysVar -> IO Limit
getSysVar v =
    case v of
      ArgumentLimit -> sysconf ``_SC_ARG_MAX''
      ChildLimit    -> sysconf ``_SC_CHILD_MAX''
      ClockTick	    -> sysconf ``_SC_CLK_TCK''
      GroupLimit    -> sysconf ``_SC_NGROUPS_MAX''
      OpenFileLimit -> sysconf ``_SC_OPEN_MAX''
      PosixVersion  -> sysconf ``_SC_VERSION''
      HasSavedIDs   -> sysconf ``_SC_SAVED_IDS''
      HasJobControl -> sysconf ``_SC_JOB_CONTROL''
--  where

sysconf :: Int -> IO Limit
sysconf n = do
 rc <- _ccall_ sysconf n
 if rc /= (-1::Int)
    then return rc
    else ioError (IOError Nothing NoSuchThing
		          "getSysVar" 
		          "no such system limit or option")

\end{code}
