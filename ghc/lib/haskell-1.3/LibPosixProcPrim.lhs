%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixProcPrim]{Haskell 1.3 POSIX Process Primitives}

\begin{code}
module LibPosixProcPrim (
    Handler(..),
    SignalSet(..),
    Signal(..),
    ProcessStatus(..),

    addSignal,
    awaitSignal,
    backgroundRead,
    backgroundWrite,
    blockSignals,
    continueProcess,
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
    sigCONT,
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

import PreludeGlaMisc
import PreludeGlaST
import PreludeStdIO
import PS

import LibPosixErr
import LibPosixUtil

import LibSystem(ExitCode(..))
import LibPosixProcEnv (getProcessID)

forkProcess :: IO (Maybe ProcessID)
forkProcess =
    _ccall_ fork				    `thenPrimIO` \ pid ->
    case pid of
      -1 -> syserr "forkProcess"
      0  -> return Nothing
      _  -> return (Just pid)

executeFile :: FilePath			    -- Command
            -> Bool			    -- Search PATH?
            -> [String]			    -- Arguments
            -> Maybe [(String, String)]	    -- Environment
            -> IO ()
executeFile path search args Nothing =
    _packBytesForCST path			    `thenStrictlyST` \ prog ->
    vectorize (basename path:args)		    `thenPrimIO` \ argv ->
    (if search then
        _casm_ ``%r = execvp(%0,(char **)%1);'' prog argv
    else
        _casm_ ``%r = execv(%0,(char **)%1);'' prog argv
    )						    `thenPrimIO` \ rc ->
    syserr "executeFile"

executeFile path search args (Just env) =
    _packBytesForCST path			    `thenStrictlyST` \ prog ->
    vectorize (basename path:args)		    `thenPrimIO` \ argv ->
    vectorize (map (\ (name, val) -> name ++ ('=' : val)) env)
						    `thenPrimIO` \ envp ->
    (if search then
        _casm_ ``%r = execvpe(%0,(char **)%1,(char **)%2);'' prog argv envp
    else
        _casm_ ``%r = execve(%0,(char **)%1,(char **)%2);'' prog argv envp
    )						    `thenPrimIO` \ rc ->
    syserr "executeFile"

data ProcessStatus = Exited ExitCode 
                   | Terminated Signal 
                   | Stopped Signal
{- mattson -}      deriving (Eq, Ord, Text)

getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid = 
    allocWords 1				    `thenPrimIO` \ wstat ->
    _casm_ ``%r = waitpid(%0, (int *)%1, %2);'' pid wstat (waitOptions block stopped)
						    `thenPrimIO` \ pid ->
    case pid of
      -1 -> syserr "getProcessStatus"
      0  -> return Nothing
      _  -> decipherWaitStatus wstat		    `thenPrimIO` \ ps ->
	    return (Just ps)

getGroupProcessStatus :: Bool 
                      -> Bool 
                      -> ProcessGroupID 
                      -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid = 
    allocWords 1				    `thenPrimIO` \ wstat ->
    _casm_ ``%r = waitpid(%0, (int *)%1, %2);'' (-pgid) wstat (waitOptions block stopped)
						    `thenPrimIO` \ pid ->
    case pid of
      -1 -> syserr "getGroupProcessStatus"
      0  -> return Nothing
      _  -> decipherWaitStatus wstat		    `thenPrimIO` \ ps ->
	    return (Just (pid, ps))

getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped = 
    getGroupProcessStatus block stopped 1	    `handle`
    \ err -> syserr "getAnyProcessStatus"

exitImmediately :: ExitCode -> IO ()
exitImmediately exitcode = 
    _ccall_ _exit (exitcode2Int exitcode)	    `thenPrimIO` \ () ->
    syserr "exitImmediately"
  where
    exitcode2Int ExitSuccess = 0
    exitcode2Int (ExitFailure n) = n

getEnvironment :: IO [(String, String)]
getEnvironment =
    unvectorize ``environ'' 0			    `thenPrimIO` \ env ->
    return (map (split "") env)
  where
    split :: String -> String -> (String, String)
    split x ('=' : xs) = (reverse x, xs)
    split x (c:cs) = split (c:x) cs

setEnvironment :: [(String, String)] -> IO ()
setEnvironment pairs =
    vectorize (map (\ (var,val) -> var ++ ('=' : val)) pairs)
						    `thenPrimIO` \ env ->
    _casm_ ``%r = setenviron((char **)%0);'' env    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setEnvironment"

getEnvVar :: String -> IO String
getEnvVar name =
    _packBytesForCST name			    `thenStrictlyST` \ str ->
    _ccall_ getenv str				    `thenPrimIO` \ str ->
    if str == ``NULL'' then
	failWith (NoSuchThing "no such environment variable")
    else
	strcpy str				    `thenPrimIO` \ env ->
	return env
    
setEnvVar :: String -> String -> IO ()
setEnvVar name value =
    _packBytesForCST (name ++ ('=' : value))	    `thenStrictlyST` \ str ->
    _ccall_ setenv str				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setEnvVar"
    
removeEnvVar :: String -> IO ()
removeEnvVar name =
    _packBytesForCST name			    `thenStrictlyST` \ str ->
    _ccall_ delenv str				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "removeEnvVar"

type Signal = Int

nullSignal :: Signal
nullSignal = 0

backgroundRead, sigTTIN :: Signal
backgroundRead = ``SIGTTIN''
sigTTIN = ``SIGTTIN''

backgroundWrite, sigTTOU :: Signal
backgroundWrite = ``SIGTTOU''
sigTTOU = ``SIGTTOU''

continueProcess, sigCONT :: Signal
continueProcess = ``SIGCONT''
sigCONT = ``SIGCONT''

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
    _ccall_ kill pid int			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "signalProcess"

raiseSignal :: Signal -> IO ()
raiseSignal int = getProcessID >>= signalProcess int

signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup int pgid = signalProcess int (-pgid)

setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag b = 
    _casm_ ``%r = nocldstop; nocldstop = %0;'' x    `thenPrimIO` \ rc ->
    return (rc == 0)
  where
    x = case b of {True -> 0; False -> 1}

queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = 
    _casm_ ``%r = nocldstop;''			    `thenPrimIO` \ rc ->
    return (rc == 0)

data Handler = Default 
             | Ignore 
             | Catch (IO ())

type SignalSet = _ByteArray ()

sigSetSize :: Int
sigSetSize = ``sizeof(sigset_t)''

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformPrimIO (
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``(void) sigemptyset((sigset_t *)%0);'' bytes
						    `thenPrimIO` \ () -> 
    freeze bytes				    `thenStrictlyST` \ sigset ->
    returnPrimIO sigset
    )

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformPrimIO (
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``(void) sigfillset((sigset_t *)%0);'' bytes
						    `thenPrimIO` \ () -> 
    freeze bytes				    `thenStrictlyST` \ sigset ->
    returnPrimIO sigset
    )

addSignal :: Signal -> SignalSet -> SignalSet
addSignal int oldset = unsafePerformPrimIO (
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``*(sigset_t *)%0 = *(sigset_t *)%1; (void) sigaddset((sigset_t *)%0, %2);'' 
           bytes oldset int			    `thenPrimIO` \ () -> 
    freeze bytes				    `thenStrictlyST` \ newset ->
    returnPrimIO newset
    )

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet int sigset = unsafePerformPrimIO (
    _casm_ ``%r = sigismember((sigset_t *)%0, %1);'' sigset int
						    `thenPrimIO` \ rc -> 
    if rc == 1 then
	returnPrimIO True
    else
	returnPrimIO False
    )

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal int oldset = unsafePerformPrimIO (
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``*(sigset_t *)%0 = *(sigset_t *)%1; (void) sigdelset((sigset_t *)%0, %2);'' 
           bytes oldset int			    `thenPrimIO` \ () -> 
    freeze bytes				    `thenStrictlyST` \ newset ->
    returnPrimIO newset
    )

installHandler :: Signal
               -> Handler 
               -> Maybe SignalSet	-- other signals to block
               -> IO Handler		-- old handler

#ifdef __PARALLEL_HASKELL__
installHandler = error "installHandler: not available for Parallel Haskell"
#else
installHandler int handler maybe_mask = (
    case handler of
      Default -> _ccall_ stg_sig_ignore int mask
      Ignore -> _ccall_ stg_sig_default int mask
      Catch m ->
        makeStablePtr (wrap m)			    `thenPrimIO` \ sptr ->
	_ccall_ stg_sig_catch int sptr mask
    )
						    `thenPrimIO` \ rc ->
    if rc >= 0 then
        _casm_ ``%r = (StgStablePtr) (%0);'' rc	    `thenPrimIO` \ osptr ->
        deRefStablePtr osptr			    `thenPrimIO` \ m ->
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
    wrap :: IO () -> PrimIO ()
    wrap m s =
        case (m s) of
	  (result, s2@(S# _)) ->
	    case result of
              Right ()  -> ( (), s2 )
              Left  err -> error ("I/O error: "++shows err "\n")

#endif {-!__PARALLEL_HASKELL__-}

getSignalMask :: IO SignalSet
getSignalMask = 
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = sigprocmask(0, NULL, (sigset_t *)%0);'' bytes
						    `thenPrimIO` \ rc ->
    if rc == 0 then
        freeze bytes				    `thenStrictlyST` \ sigset ->
        return sigset
    else
	syserr "getSignalMask"

sigProcMask :: String -> Int -> SignalSet -> IO SignalSet
sigProcMask name how sigset =
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = sigprocmask(%0, (sigset_t *)%1, (sigset_t *)%2);'' how sigset bytes
						    `thenPrimIO` \ rc ->
    if rc == 0 then
        freeze bytes				    `thenStrictlyST` \ oldset ->
        return oldset
    else
	syserr name

setSignalMask :: SignalSet -> IO SignalSet
setSignalMask = sigProcMask "setSignalMask" ``SIG_SETMASK''

blockSignals :: SignalSet -> IO SignalSet
blockSignals = sigProcMask "blockSignals" ``SIG_BLOCK''

unBlockSignals :: SignalSet -> IO SignalSet
unBlockSignals = sigProcMask "unBlockSignals" ``SIG_UNBLOCK''

getPendingSignals :: IO SignalSet 
getPendingSignals =
    allocChars sigSetSize			    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = sigpending((sigset_t *)%0);'' bytes    
						    `thenPrimIO` \ rc ->
    if rc == 0 then
        freeze bytes				    `thenStrictlyST` \ sigset ->
        return sigset
    else
	syserr "getPendingSignals"

awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset =
    pause					    `thenPrimIO` \ () ->
    getErrorCode				    >>= \ err ->
    if err == interruptedOperation then
        return ()
    else
	syserr "awaitSignal"
  where
    pause :: PrimIO ()
    pause = 
        case maybe_sigset of
          Nothing -> _casm_ ``(void) pause();''
          Just sigset -> _casm_ ``(void) sigsuspend((sigset_t *)%0);'' sigset

scheduleAlarm :: Int -> IO Int
scheduleAlarm (I# secs#) =
    _ccall_ alarm (W# (int2Word# secs#))	    `thenPrimIO` \ (W# w#) ->
    return (I# (word2Int# w#))

sleep :: Int -> IO ()
sleep 0 = return ()
sleep (I# secs#) = 
    _ccall_ sleep (W# (int2Word# secs#))	    `seqPrimIO`
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

decipherWaitStatus :: _MutableByteArray s x -> PrimIO ProcessStatus
decipherWaitStatus wstat =
    _casm_ ``%r = WIFEXITED(*(int *)%0);'' wstat    `thenPrimIO` \ exited ->
    if exited /= 0 then
        _casm_ ``%r = WEXITSTATUS(*(int *)%0);'' wstat
						    `thenPrimIO` \ exitstatus ->
        if exitstatus == 0 then
	    returnPrimIO (Exited ExitSuccess)
	else
	    returnPrimIO (Exited (ExitFailure exitstatus))
    else
        _casm_ ``%r = WIFSIGNALED(*(int *)%0);'' wstat  
						    `thenPrimIO` \ signalled ->
        if signalled /= 0 then
            _casm_ ``%r = WTERMSIG(*(int *)%0);'' wstat
						    `thenPrimIO` \ termsig ->
	    returnPrimIO (Terminated termsig)
	else
            _casm_ ``%r = WSTOPSIG(*(int *)%0);'' wstat
						    `thenPrimIO` \ stopsig ->
	    returnPrimIO (Stopped stopsig)

\end{code}
