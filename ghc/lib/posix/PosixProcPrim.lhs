%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[PosixProcPrim]{Haskell 1.3 POSIX Process Primitives}

\begin{code}

#include "config.h"

module PosixProcPrim (
    Handler(..),
    SignalSet,
    Signal,
    ProcessStatus(..),

    addSignal,
    awaitSignal,
    backgroundRead,
    backgroundWrite,
    blockSignals,
#ifndef cygwin32_TARGET_OS
    continueProcess,
#endif
    deleteSignal,
    emptySignalSet,
    executeFile,
    exitImmediately,
    floatingPointException,
    forkProcess,
    fullSignalSet,
    getAnyProcessStatus,
    getEnvVar,
    getEnvironment,
    getGroupProcessStatus,
    getPendingSignals,
    getProcessStatus,
    getSignalMask,
    illegalInstruction,
    inSignalSet,
    installHandler,
    internalAbort,
    keyboardSignal,
    keyboardStop,
    keyboardTermination,
    killProcess,
    lostConnection,
    nullSignal,
    openEndedPipe,
    processStatusChanged,
    queryStoppedChildFlag,
    raiseSignal,
    realTimeAlarm,
    removeEnvVar,
    scheduleAlarm,
    segmentationViolation,
    setEnvVar,
    setEnvironment,
    setSignalMask,
    setStoppedChildFlag,
    sigABRT,
    sigALRM,
    sigCHLD,
#ifndef cygwin32_TARGET_OS
    sigCONT,
#endif
    sigFPE,
    sigHUP,
    sigILL,
    sigINT,
    sigKILL,
    sigPIPE,
    sigProcMask,
    sigQUIT,
    sigSEGV,
    sigSTOP,
    sigSetSize,
    sigTERM,
    sigTSTP,
    sigTTIN,
    sigTTOU,
    sigUSR1,
    sigUSR2,
    signalProcess,
    signalProcessGroup,
    sleep,
    softwareStop,
    softwareTermination,
    unBlockSignals,
    userDefinedSignal1,
    userDefinedSignal2,

    ExitCode

    ) where

import GlaExts
import IO
import PrelIOBase
import PackedString (psToByteArrayST)
import Foreign  -- stable pointers
import PosixErr
import PosixUtil

import System(ExitCode(..))
import PosixProcEnv (getProcessID)

forkProcess :: IO (Maybe ProcessID)
forkProcess = do
    pid <-_ccall_ fork
    case pid of
      -1 -> syserr "forkProcess"
      0  -> return Nothing
      _  -> return (Just pid)

executeFile :: FilePath			    -- Command
            -> Bool			    -- Search PATH?
            -> [String]			    -- Arguments
            -> Maybe [(String, String)]	    -- Environment
            -> IO ()
executeFile path search args Nothing = do
    prog <- psToByteArrayIO path
    argv <- vectorize (basename path:args)
    (if search then
        _casm_ ``execvp(%0,(char **)%1);'' prog argv
     else
        _casm_ ``execv(%0,(char **)%1);'' prog argv
     )
    syserr "executeFile"

executeFile path search args (Just env) = do
    prog <- psToByteArrayIO path
    argv <- vectorize (basename path:args)
    envp <- vectorize (map (\ (name, val) -> name ++ ('=' : val)) env)
    (if search then
        _casm_ `` execvpe(%0,(char **)%1,(char **)%2);'' prog argv envp
     else
        _casm_ `` execve(%0,(char **)%1,(char **)%2);'' prog argv envp
     )
    syserr "executeFile"

data ProcessStatus = Exited ExitCode
                   | Terminated Signal
                   | Stopped Signal
		   deriving (Eq, Ord, Show)

getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid = do
    wstat <- allocWords 1
    pid   <-_casm_ ``%r = waitpid(%0, (int *)%1, %2);'' pid wstat
		(waitOptions block stopped)
    case pid of
      -1 -> syserr "getProcessStatus"
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstat
	       return (Just ps)

getGroupProcessStatus :: Bool
                      -> Bool
                      -> ProcessGroupID
                      -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid = do
    wstat <- allocWords 1
    pid   <-_casm_ ``%r = waitpid(%0, (int *)%1, %2);'' (-pgid) wstat
		   (waitOptions block stopped)
    case pid of
      -1 -> syserr "getGroupProcessStatus"
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstat
	       return (Just (pid, ps))

getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped =
    getGroupProcessStatus block stopped 1	    `catch`
    \ err -> syserr "getAnyProcessStatus"

exitImmediately :: ExitCode -> IO ()
exitImmediately exitcode = do
    _casm_ ``_exit(%0);'' (exitcode2Int exitcode)
    syserr "exitImmediately"
  where
    exitcode2Int ExitSuccess = 0
    exitcode2Int (ExitFailure n) = n

getEnvironment :: IO [(String, String)]
getEnvironment = do
    env <- unvectorize ``environ'' 0
    return (map (split "") env)
  where
    split :: String -> String -> (String, String)
    split x [] = error ("PosixProcPrim.getEnvironment:no `='? in: "++reverse x)
    split x ('=' : xs) = (reverse x, xs)
    split x (c:cs) = split (c:x) cs

setEnvironment :: [(String, String)] -> IO ()
setEnvironment pairs = do
    env <- vectorize (map (\ (var,val) -> var ++ ('=' : val)) pairs)
    nonzero_error (_casm_ ``%r = setenviron((char **)%0);'' env)
	"setEnvironment"

getEnvVar :: String -> IO String
getEnvVar name = do
    str <- psToByteArrayIO name
    str <- _ccall_ getenv str
    if str == ``NULL''
       then fail (IOError Nothing NoSuchThing
		 "getEnvVar: no such environment variable")
       else strcpy str

setEnvVar :: String -> String -> IO ()
setEnvVar name value = do
    str <- psToByteArrayIO (name ++ ('=' : value))
    nonzero_error (_casm_ ``%r = _setenv(%0);'' str) "setEnvVar"

removeEnvVar :: String -> IO ()
removeEnvVar name = do
    str <- psToByteArrayIO name
    nonzero_error (_ccall_ delenv str) "removeEnvVar"

type Signal = Int

nullSignal :: Signal
nullSignal = 0

backgroundRead, sigTTIN :: Signal
backgroundRead = ``SIGTTIN''
sigTTIN = ``SIGTTIN''

backgroundWrite, sigTTOU :: Signal
backgroundWrite = ``SIGTTOU''
sigTTOU = ``SIGTTOU''

#ifndef cygwin32_TARGET_OS
continueProcess, sigCONT :: Signal
continueProcess = ``SIGCONT''
sigCONT = ``SIGCONT''
#endif

floatingPointException, sigFPE :: Signal
floatingPointException = ``SIGFPE''
sigFPE = ``SIGFPE''

illegalInstruction, sigILL :: Signal
illegalInstruction = ``SIGILL''
sigILL = ``SIGILL''

internalAbort, sigABRT ::Signal
internalAbort = ``SIGABRT''
sigABRT = ``SIGABRT''

keyboardSignal, sigINT :: Signal
keyboardSignal = ``SIGINT''
sigINT = ``SIGINT''

keyboardStop, sigTSTP :: Signal
keyboardStop = ``SIGTSTP''
sigTSTP = ``SIGTSTP''

keyboardTermination, sigQUIT :: Signal
keyboardTermination = ``SIGQUIT''
sigQUIT = ``SIGQUIT''

killProcess, sigKILL :: Signal
killProcess = ``SIGKILL''
sigKILL = ``SIGKILL''

lostConnection, sigHUP :: Signal
lostConnection = ``SIGHUP''
sigHUP = ``SIGHUP''

openEndedPipe, sigPIPE :: Signal
openEndedPipe = ``SIGPIPE''
sigPIPE = ``SIGPIPE''

processStatusChanged, sigCHLD :: Signal
processStatusChanged = ``SIGCHLD''
sigCHLD = ``SIGCHLD''

realTimeAlarm, sigALRM :: Signal
realTimeAlarm = ``SIGALRM''
sigALRM = ``SIGALRM''

segmentationViolation, sigSEGV :: Signal
segmentationViolation = ``SIGSEGV''
sigSEGV = ``SIGSEGV''

softwareStop, sigSTOP :: Signal
softwareStop = ``SIGSTOP''
sigSTOP = ``SIGSTOP''

softwareTermination, sigTERM :: Signal
softwareTermination = ``SIGTERM''
sigTERM = ``SIGTERM''

userDefinedSignal1, sigUSR1 :: Signal
userDefinedSignal1 = ``SIGUSR1''
sigUSR1 = ``SIGUSR1''

userDefinedSignal2, sigUSR2 :: Signal
userDefinedSignal2 = ``SIGUSR2''
sigUSR2 = ``SIGUSR2''

signalProcess :: Signal -> ProcessID -> IO ()
signalProcess int pid =
    nonzero_error (_ccall_ kill pid int) "signalProcess"

raiseSignal :: Signal -> IO ()
raiseSignal int = getProcessID >>= signalProcess int

signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup int pgid = signalProcess int (-pgid)

setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag b = do
    rc <- _casm_ ``%r = nocldstop; nocldstop = %0;'' x
    return (rc == 0)
  where
    x = case b of {True -> 0; False -> 1}

queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = do
    rc <- _casm_ ``%r = nocldstop;''
    return (rc == 0)

data Handler = Default
             | Ignore
             | Catch (IO ())

type SignalSet = ByteArray ()

sigSetSize :: Int
sigSetSize = ``sizeof(sigset_t)''

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformPrimIO $ do
    bytes <- allocChars sigSetSize
    _casm_ ``(void) sigemptyset((sigset_t *)%0);'' bytes
    freeze bytes

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformPrimIO $ do
    bytes <- allocChars sigSetSize
    _casm_ ``(void) sigfillset((sigset_t *)%0);'' bytes
    freeze bytes

addSignal :: Signal -> SignalSet -> SignalSet
addSignal int oldset = unsafePerformPrimIO $ do
    bytes <- allocChars sigSetSize
    _casm_ ``*(sigset_t *)%0 = *(sigset_t *)%1;
	     (void) sigaddset((sigset_t *)%0, %2);''
	bytes oldset int
    freeze bytes

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet int sigset = unsafePerformPrimIO $ do
    rc <- _casm_ ``%r = sigismember((sigset_t *)%0, %1);'' sigset int
    return (rc == 1)

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal int oldset = unsafePerformPrimIO $ do
    bytes <- allocChars sigSetSize
    _casm_ ``*(sigset_t *)%0 = *(sigset_t *)%1;
	     (void) sigdelset((sigset_t *)%0, %2);''
           bytes oldset int
    freeze bytes

installHandler :: Signal
               -> Handler
               -> Maybe SignalSet	-- other signals to block
               -> IO Handler		-- old handler

#ifdef __PARALLEL_HASKELL__
installHandler = error "installHandler: not available for Parallel Haskell"
#else
installHandler int handler maybe_mask = (
    case handler of
      Default -> _ccall_ stg_sig_default int mask
      Ignore  -> _ccall_ stg_sig_ignore  int mask
      Catch m -> do
        sptr <- makeStablePtr (ioToPrimIO m)
	_ccall_ stg_sig_catch int sptr mask
    ) >>= \rc ->

    if rc >= 0 then do
        osptr <- _casm_ ``%r = (StgStablePtr) (%0);'' rc
        m     <- deRefStablePtr osptr
	return (Catch m)
    else if rc == ``STG_SIG_DFL'' then
	return Default
    else if rc == ``STG_SIG_IGN'' then
	return Ignore
    else
	syserr "installHandler"
  where
    mask = case maybe_mask of
	     Nothing -> emptySignalSet
             Just x -> x

#endif {-!__PARALLEL_HASKELL__-}

getSignalMask :: IO SignalSet
getSignalMask = do
    bytes <- allocChars sigSetSize
    rc    <- _casm_ ``%r = sigprocmask(0, NULL, (sigset_t *)%0);'' bytes
    if rc == 0
       then freeze bytes
       else syserr "getSignalMask"

sigProcMask :: String -> Int -> SignalSet -> IO SignalSet
sigProcMask name how sigset = do
    bytes <- allocChars sigSetSize
    rc <- _casm_ ``%r = sigprocmask(%0, (sigset_t *)%1, (sigset_t *)%2);''
		 how sigset bytes
    if rc == 0
       then freeze bytes
       else syserr name

setSignalMask :: SignalSet -> IO SignalSet
setSignalMask = sigProcMask "setSignalMask" ``SIG_SETMASK''

blockSignals :: SignalSet -> IO SignalSet
blockSignals = sigProcMask "blockSignals" ``SIG_BLOCK''

unBlockSignals :: SignalSet -> IO SignalSet
unBlockSignals = sigProcMask "unBlockSignals" ``SIG_UNBLOCK''

getPendingSignals :: IO SignalSet
getPendingSignals = do
    bytes <- allocChars sigSetSize
    rc <- _casm_ ``%r = sigpending((sigset_t *)%0);'' bytes
    if rc == 0
       then freeze bytes
       else syserr "getPendingSignals"

awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset = do
    pause maybe_sigset
    err <- getErrorCode
    if err == interruptedOperation
       then return ()
       else syserr "awaitSignal"
--  where

pause :: Maybe SignalSet -> IO ()
pause maybe_sigset =
  case maybe_sigset of
   Nothing -> _casm_ ``(void) pause();''
   Just sigset -> _casm_ ``(void) sigsuspend((sigset_t *)%0);'' sigset

scheduleAlarm :: Int -> IO Int
scheduleAlarm (I# secs#) =
    _ccall_ alarm (W# (int2Word# secs#))	    >>= \ (W# w#) ->
    return (I# (word2Int# w#))

sleep :: Int -> IO ()
sleep 0 = return ()
sleep (I# secs#) = do
    _ccall_ sleep (W# (int2Word# secs#))
    return ()
\end{code}

Local utility functions

\begin{code}

-- Get the trailing component of a path

basename :: String -> String
basename "" = ""
basename (c:cs)
  | c == '/' = basename cs
  | otherwise = c : basename cs

-- Convert wait options to appropriate set of flags

waitOptions :: Bool -> Bool -> Int
--             block   stopped
waitOptions False False = ``WNOHANG''
waitOptions False True  = ``(WNOHANG|WUNTRACED)''
waitOptions True  False = 0
waitOptions True  True  = ``WUNTRACED''

-- Turn a (ptr to a) wait status into a ProcessStatus

decipherWaitStatus :: MutableByteArray s x -> IO ProcessStatus
decipherWaitStatus wstat = do
    exited <- _casm_ ``%r = WIFEXITED(*(int *)%0);'' wstat
    if exited /= 0
      then do
        exitstatus <- _casm_ ``%r = WEXITSTATUS(*(int *)%0);'' wstat
        if exitstatus == 0
	   then return (Exited ExitSuccess)
	   else return (Exited (ExitFailure exitstatus))
      else do
        signalled <- _casm_ ``%r = WIFSIGNALED(*(int *)%0);'' wstat
        if signalled /= 0
	   then do
		termsig <- _casm_ ``%r = WTERMSIG(*(int *)%0);'' wstat
		return (Terminated termsig)
	   else do
		stopsig <-_casm_ ``%r = WSTOPSIG(*(int *)%0);'' wstat
		return (Stopped stopsig)
\end{code}
