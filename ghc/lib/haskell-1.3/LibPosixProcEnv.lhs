%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixProcEnv]{Haskell 1.3 POSIX Process Environment}

\begin{code}
module LibPosixProcEnv (
    ProcessTimes(..),
    SysVar(..),
    SystemID(..),

    childSystemTime,
    childUserTime,
    createProcessGroup,
    createSession,
    elapsedTime,
    epochTime,
    getControllingTerminalName,
    getEffectiveGroupID,
    getEffectiveUserID,
    getEffectiveUserName,
    getGroups,
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

import PreludeGlaST
import PS

import LibPosixErr
import LibPosixUtil

getProcessID :: IO ProcessID
getProcessID = 
    _ccall_ getpid				    `thenPrimIO` \ pid ->
    return pid

getParentProcessID :: IO ProcessID
getParentProcessID =
    _ccall_ getppid				    `thenPrimIO` \ ppid ->
    return ppid

getRealUserID :: IO UserID
getRealUserID =
    _ccall_ getuid				    `thenPrimIO` \ uid ->
    return uid

getEffectiveUserID :: IO UserID
getEffectiveUserID =
    _ccall_ geteuid				    `thenPrimIO` \ euid ->
    return euid

setUserID :: UserID -> IO ()
setUserID uid = 
    _ccall_ setuid uid				    `thenPrimIO` \ rc ->
    if rc == 0 then
        return ()
    else
	syserr "setUserID"

getLoginName :: IO String
getLoginName = 
    _ccall_ getlogin				    `thenPrimIO` \ str ->
    if str == ``NULL'' then
	syserr "getLoginName"
    else
	strcpy str				    `thenPrimIO` \ name ->
	return name

getRealGroupID :: IO GroupID
getRealGroupID = 
    _ccall_ getgid				    `thenPrimIO` \ gid ->
    return gid

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID =
    _ccall_ getegid				    `thenPrimIO` \ egid ->
    return egid

setGroupID :: GroupID -> IO ()
setGroupID gid = 
    _ccall_ setgid gid				    `thenPrimIO` \ rc ->
    if rc == 0 then
        return ()
    else
	syserr "setGroupID"

getGroups :: IO [GroupID]
getGroups = 
    _ccall_ getgroups 0 (``NULL''::_Addr)	    `thenPrimIO` \ ngroups ->
    allocWords ngroups				    `thenStrictlyST` \ words ->
    _casm_ ``%r = getgroups(%0,(gid_t *)%1);'' ngroups words
						    `thenPrimIO` \ ngroups ->
    if ngroups /= -1 then
	freeze words				    `thenStrictlyST` \ arr ->
        return (map (extract arr) [0..(ngroups-1)])
    else
	syserr "getGroups"
  where
    extract (_ByteArray _ barr#) (I# n#) =
        case indexIntArray# barr# n# of
	  r# -> (I# r#)

getEffectiveUserName :: IO String
getEffectiveUserName =
    _ccall_ cuserid (``NULL''::_Addr)		    `thenPrimIO` \ str ->
    if str == ``NULL'' then
	syserr "getEffectiveUserName"
    else
	strcpy str				    `thenPrimIO` \ name ->
	return name

getProcessGroupID :: IO ProcessGroupID
getProcessGroupID =
    _ccall_ getpgrp				    `thenPrimIO` \ pgid ->
    return pgid

createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = 
    _ccall_ setpgid pid 0			    `thenPrimIO` \ pgid ->
    if pgid == 0 then
	return pgid
    else
	syserr "createProcessGroup"

joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid = 
    _ccall_ setpgid 0 pgid			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setProcessGroupID"

setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid = 
    _ccall_ setpgid pid pgid			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setProcessGroupID"

createSession :: IO ProcessGroupID
createSession = 
    _ccall_ setsid				    `thenPrimIO` \ pgid ->
    if pgid /= -1 then
	return pgid
    else
	syserr "createSession"

type SystemID = _ByteArray ()

systemName :: SystemID -> String
systemName sid =  unsafePerformPrimIO (
    _casm_ ``%r = ((struct utsname *)%0)->sysname;'' sid
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ sysname ->
    returnPrimIO sysname)

nodeName :: SystemID -> String
nodeName sid =  unsafePerformPrimIO (
    _casm_ ``%r = ((struct utsname *)%0)->nodename;'' sid
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ nodename ->
    returnPrimIO nodename)

release :: SystemID -> String
release sid =  unsafePerformPrimIO (
    _casm_ ``%r = ((struct utsname *)%0)->release;'' sid
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ releaseStr ->
    returnPrimIO releaseStr)

version :: SystemID -> String
version sid =  unsafePerformPrimIO (
    _casm_ ``%r = ((struct utsname *)%0)->version;'' sid
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ versionStr ->
    returnPrimIO versionStr)

machine :: SystemID -> String
machine sid = unsafePerformPrimIO (
    _casm_ ``%r = ((struct utsname *)%0)->machine;'' sid
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ machine ->
    returnPrimIO machine)

getSystemID :: IO SystemID
getSystemID = 
    allocChars (``sizeof(struct utsname)''::Int)    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = uname((struct utsname *)%0);'' bytes
						    `thenPrimIO` \ rc ->
    if rc /= -1 then
	freeze bytes				    `thenStrictlyST` \ sid ->
	return sid
    else
	syserr "getSystemID"

epochTime :: IO EpochTime
epochTime = 
    _ccall_ time (``NULL''::_Addr)		    `thenPrimIO` \ secs ->
    if secs /= -1 then
	return secs
    else
	syserr "epochTime"

-- All times in clock ticks (see getClockTick)

type ProcessTimes = (ClockTick, _ByteArray ())

elapsedTime :: ProcessTimes -> ClockTick
elapsedTime (realtime, _) = realtime

userTime :: ProcessTimes -> ClockTick
userTime (_, times) = unsafePerformPrimIO (
    _casm_ ``%r = ((struct tms *)%0)->tms_utime;'' times
						    `thenStrictlyST` \ utime ->
    returnPrimIO utime)

systemTime :: ProcessTimes -> ClockTick
systemTime (_, times) = unsafePerformPrimIO (
    _casm_ ``%r = ((struct tms *)%0)->tms_stime;'' times
						    `thenStrictlyST` \ stime ->
    returnPrimIO stime)

childUserTime :: ProcessTimes -> ClockTick
childUserTime (_, times) = unsafePerformPrimIO (
    _casm_ ``%r = ((struct tms *)%0)->tms_cutime;'' times
						    `thenStrictlyST` \ cutime ->
    returnPrimIO cutime)

childSystemTime :: ProcessTimes -> ClockTick
childSystemTime (_, times) = unsafePerformPrimIO (
    _casm_ ``%r = ((struct tms *)%0)->tms_cstime;'' times
						    `thenStrictlyST` \ cstime ->
    returnPrimIO cstime)

getProcessTimes :: IO ProcessTimes
getProcessTimes =
    allocChars (``sizeof(struct tms)''::Int)	    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = times((struct tms *)%0);'' bytes  `thenPrimIO` \ elapsed ->
    if elapsed /= -1 then
	freeze bytes				    `thenStrictlyST` \ times ->
	return (elapsed, times)
    else
	syserr "getProcessTimes"

getControllingTerminalName :: IO FilePath
getControllingTerminalName = 
    _ccall_ ctermid (``NULL''::_Addr)		    `thenPrimIO` \ str ->
    if str == ``NULL'' then
	failWith (NoSuchThing "no controlling terminal")
    else
	strcpy str				    `thenPrimIO` \ name ->
	return name

getTerminalName :: Channel -> IO FilePath
getTerminalName fd = 
    _ccall_ ttyname fd				    `thenPrimIO` \ str ->
    if str == ``NULL'' then
        try (queryTerminal fd)			    >>=
        either (\err -> syserr "getTerminalName") 
               (\succ -> if succ then failWith (NoSuchThing "terminal name")
                         else failWith (InappropriateType "not a terminal"))
    else
	strcpy str				    `thenPrimIO` \ name ->
	return name

queryTerminal :: Channel -> IO Bool
queryTerminal fd =
    _ccall_ isatty fd				    `thenPrimIO` \ rc ->
    case rc of
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
      ChildLimit -> sysconf ``_SC_CHILD_MAX''
      ClockTick -> sysconf ``_SC_CLK_TCK''
      GroupLimit -> sysconf ``_SC_NGROUPS_MAX''
      OpenFileLimit -> sysconf ``_SC_OPEN_MAX''
      PosixVersion -> sysconf ``_SC_VERSION''
      HasSavedIDs -> sysconf ``_SC_SAVED_IDS''
      HasJobControl -> sysconf ``_SC_JOB_CONTROL''
  where
    sysconf :: Int -> IO Limit
    sysconf n =
	_ccall_ sysconf n			    `thenPrimIO` \ rc ->
        if rc /= -1 then
            return rc
        else
	    failWith (NoSuchThing "no such system limit or option")

\end{code}
