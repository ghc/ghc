-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX signal support
--
-----------------------------------------------------------------------------

#include "config.h"

module System.Posix.Signals (
#ifndef mingw32_TARGET_OS
  -- * The Signal type
  Signal,

  -- * Specific signals
  nullSignal,
  internalAbort, sigABRT,
  realTimeAlarm, sigALRM,
  busError, sigBUS,
  processStatusChanged, sigCHLD,
  continueProcess, sigCONT,
  floatingPointException, sigFPE,
  lostConnection, sigHUP,
  illegalInstruction, sigILL,
  keyboardSignal, sigINT,
  killProcess, sigKILL,
  openEndedPipe, sigPIPE,
  keyboardTermination, sigQUIT,
  segmentationViolation, sigSEGV,
  softwareStop, sigSTOP,
  softwareTermination, sigTERM,
  keyboardStop, sigTSTP,
  backgroundRead, sigTTIN,
  backgroundWrite, sigTTOU,
  userDefinedSignal1, sigUSR1,
  userDefinedSignal2, sigUSR2,
  pollableEvent, sigPOLL,
  profilingTimerExpired, sigPROF,
  badSystemCall, sigSYS,
  breakpointTrap, sigTRAP,
  urgentDataAvailable, sigURG,
  virtualTimerExpired, sigVTALRM,
  cpuTimeLimitExceeded, sigXCPU,
  fileSizeLimitExceeded, sigXFSZ,

  -- * Sending signals
  raiseSignal,
  signalProcess,
  signalProcessGroup,

  -- * Handling signals
  Handler(..),
  installHandler,

  -- * Signal sets
  SignalSet,
  emptySignalSet, fullSignalSet, 
  addSignal, deleteSignal, inSignalSet,

  -- * The process signal mask
  getSignalMask, setSignalMask, blockSignals, unblockSignals,

  -- * The alarm timer
  scheduleAlarm,

  -- * Waiting for signals
  getPendingSignals, awaitSignal,

  -- MISSING FUNCTIONALITY:
  -- sigaction(), (inc. the sigaction structure + flags etc.)
  -- the siginfo structure
  -- sigaltstack()
  -- sighold, sigignore, sigpause, sigrelse, sigset
  -- siginterrupt
#endif
  ) where

#include "Signals.h"

import Foreign
import Foreign.C
import System.IO.Unsafe
import System.Posix.Types
import GHC.Posix

#ifndef mingw32_TARGET_OS
-- WHOLE FILE...

-- -----------------------------------------------------------------------------
-- Specific signals

type Signal = CInt

nullSignal :: Signal
nullSignal = 0

foreign import ccall "__hsposix_SIGABRT"   sigABRT   :: CInt
foreign import ccall "__hsposix_SIGALRM"   sigALRM   :: CInt
foreign import ccall "__hsposix_SIGBUS"    sigBUS    :: CInt
foreign import ccall "__hsposix_SIGCHLD"   sigCHLD   :: CInt
foreign import ccall "__hsposix_SIGCONT"   sigCONT   :: CInt
foreign import ccall "__hsposix_SIGFPE"    sigFPE    :: CInt
foreign import ccall "__hsposix_SIGHUP"    sigHUP    :: CInt
foreign import ccall "__hsposix_SIGILL"    sigILL    :: CInt
foreign import ccall "__hsposix_SIGINT"    sigINT    :: CInt
foreign import ccall "__hsposix_SIGKILL"   sigKILL   :: CInt
foreign import ccall "__hsposix_SIGPIPE"   sigPIPE   :: CInt
foreign import ccall "__hsposix_SIGQUIT"   sigQUIT   :: CInt
foreign import ccall "__hsposix_SIGSEGV"   sigSEGV   :: CInt
foreign import ccall "__hsposix_SIGSTOP"   sigSTOP   :: CInt
foreign import ccall "__hsposix_SIGTERM"   sigTERM   :: CInt
foreign import ccall "__hsposix_SIGTSTP"   sigTSTP   :: CInt
foreign import ccall "__hsposix_SIGTTIN"   sigTTIN   :: CInt
foreign import ccall "__hsposix_SIGTTOU"   sigTTOU   :: CInt
foreign import ccall "__hsposix_SIGUSR1"   sigUSR1   :: CInt
foreign import ccall "__hsposix_SIGUSR2"   sigUSR2   :: CInt
foreign import ccall "__hsposix_SIGPOLL"   sigPOLL   :: CInt
foreign import ccall "__hsposix_SIGPROF"   sigPROF   :: CInt
foreign import ccall "__hsposix_SIGSYS"    sigSYS    :: CInt
foreign import ccall "__hsposix_SIGTRAP"   sigTRAP   :: CInt
foreign import ccall "__hsposix_SIGURG"    sigURG    :: CInt
foreign import ccall "__hsposix_SIGVTALRM" sigVTALRM :: CInt
foreign import ccall "__hsposix_SIGXCPU"   sigXCPU   :: CInt
foreign import ccall "__hsposix_SIGXFSZ"   sigXFSZ   :: CInt

internalAbort ::Signal
internalAbort = sigABRT

realTimeAlarm :: Signal
realTimeAlarm = sigALRM

busError :: Signal
busError = sigBUS

processStatusChanged :: Signal
processStatusChanged = sigCHLD

#ifndef cygwin32_TARGET_OS
continueProcess :: Signal
continueProcess = sigCONT
#endif

floatingPointException :: Signal
floatingPointException = sigFPE

lostConnection :: Signal
lostConnection = sigHUP

illegalInstruction :: Signal
illegalInstruction = sigILL

keyboardSignal :: Signal
keyboardSignal = sigINT

killProcess :: Signal
killProcess = sigKILL

openEndedPipe :: Signal
openEndedPipe = sigPIPE

keyboardTermination :: Signal
keyboardTermination = sigQUIT

segmentationViolation :: Signal
segmentationViolation = sigSEGV

softwareStop :: Signal
softwareStop = sigSTOP

softwareTermination :: Signal
softwareTermination = sigTERM

keyboardStop :: Signal
keyboardStop = sigTSTP

backgroundRead :: Signal
backgroundRead = sigTTIN

backgroundWrite :: Signal
backgroundWrite = sigTTOU

userDefinedSignal1 :: Signal
userDefinedSignal1 = sigUSR1

userDefinedSignal2 :: Signal
userDefinedSignal2 = sigUSR2

pollableEvent :: Signal
pollableEvent = sigPOLL

profilingTimerExpired :: Signal
profilingTimerExpired = sigPROF

badSystemCall :: Signal
badSystemCall = sigSYS

breakpointTrap :: Signal
breakpointTrap = sigTRAP

urgentDataAvailable :: Signal
urgentDataAvailable = sigURG

virtualTimerExpired :: Signal
virtualTimerExpired = sigVTALRM

cpuTimeLimitExceeded :: Signal
cpuTimeLimitExceeded = sigXCPU

fileSizeLimitExceeded :: Signal
fileSizeLimitExceeded = sigXFSZ

-- -----------------------------------------------------------------------------
-- Signal-related functions

signalProcess :: Signal -> ProcessID -> IO ()
signalProcess sig pid 
 = throwErrnoIfMinus1_ "signalProcess" (c_kill (fromIntegral pid) sig)

foreign import ccall unsafe "kill"
  c_kill :: CPid -> CInt -> IO CInt

signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup sig pgid 
  = throwErrnoIfMinus1_ "signalProcessGroup" (c_killpg (fromIntegral pgid) sig)

foreign import ccall unsafe "killpg"
  c_killpg :: CPid -> CInt -> IO CInt

raiseSignal :: Signal -> IO ()
raiseSignal sig = throwErrnoIfMinus1_ "raiseSignal" (c_raise sig)

foreign import ccall unsafe "raise"
  c_raise :: CInt -> IO CInt

data Handler = Default
             | Ignore
	     -- not yet: | Hold 
             | Catch (IO ())

installHandler :: Signal
               -> Handler
               -> Maybe SignalSet	-- other signals to block
               -> IO Handler		-- old handler

#ifdef __PARALLEL_HASKELL__
installHandler = 
  error "installHandler: not available for Parallel Haskell"
#else

installHandler int handler maybe_mask = do
    case maybe_mask of
	Nothing -> install' nullPtr
        Just (SignalSet x) -> withForeignPtr x $ install' 
  where 
    install' mask = 
      alloca $ \p_sp -> do

      rc <- case handler of
      	      Default -> stg_sig_install int (#const STG_SIG_DFL) p_sp mask
      	      Ignore  -> stg_sig_install int (#const STG_SIG_IGN) p_sp mask
      	      Catch m -> do sptr <- newStablePtr m
			    poke p_sp sptr
			    stg_sig_install int (#const STG_SIG_HAN) p_sp mask 

      case rc of
	(#const STG_SIG_DFL) -> return Default
	(#const STG_SIG_IGN) -> return Ignore
	(#const STG_SIG_ERR) -> throwErrno "installHandler"
	(#const STG_SIG_HAN) -> do
		osptr <- peek p_sp
        	m     <- deRefStablePtr osptr
		return (Catch m)

foreign import ccall unsafe
  stg_sig_install :: CInt -> CInt -> Ptr (StablePtr (IO ())) -> Ptr CSigset
	 -> IO CInt

#endif // !__PARALLEL_HASKELL__

-- -----------------------------------------------------------------------------
-- Alarms

scheduleAlarm :: Int -> IO Int
scheduleAlarm secs = do
   r <- c_alarm (fromIntegral secs)
   return (fromIntegral r)

foreign import ccall unsafe "alarm"
  c_alarm :: CUInt -> IO CUInt

-- -----------------------------------------------------------------------------
-- Manipulating signal sets

newtype SignalSet = SignalSet (ForeignPtr CSigset)

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "emptySignalSet" (withForeignPtr fp $ c_sigemptyset)
  return (SignalSet fp)

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "fullSignalSet" (withForeignPtr fp $ c_sigfillset)
  return (SignalSet fp)

infixr `addSignal`, `deleteSignal`
addSignal :: Signal -> SignalSet -> SignalSet
addSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "addSignal" (c_sigaddset p2 sig)
  return (SignalSet fp2)

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "deleteSignal" (c_sigdelset p2 sig)
  return (SignalSet fp2)

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet sig (SignalSet fp) = unsafePerformIO $
  withForeignPtr fp $ \p -> do
    r <- throwErrnoIfMinus1 "inSignalSet" (c_sigismember p sig)
    return (r /= 0)

getSignalMask :: IO SignalSet
getSignalMask = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getSignalMask" (c_sigprocmask 0 p nullPtr)
  return (SignalSet fp)
   
sigProcMask :: String -> CInt -> SignalSet -> IO ()
sigProcMask fn how (SignalSet set) =
  withForeignPtr set $ \p_set ->
    throwErrnoIfMinus1_ fn (c_sigprocmask how p_set nullPtr)
  
setSignalMask :: SignalSet -> IO ()
setSignalMask set = sigProcMask "setSignalMask" c_SIG_SETMASK set

blockSignals :: SignalSet -> IO ()
blockSignals set = sigProcMask "blockSignals" c_SIG_BLOCK set

unblockSignals :: SignalSet -> IO ()
unblockSignals set = sigProcMask "unblockSignals" c_SIG_UNBLOCK set

getPendingSignals :: IO SignalSet
getPendingSignals = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p -> 
   throwErrnoIfMinus1_ "getPendingSignals" (c_sigpending p)
  return (SignalSet fp)

#ifndef cygwin32_TARGET_OS
awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset = do
  fp <- case maybe_sigset of
    	  Nothing -> do SignalSet fp <- getSignalMask; return fp
    	  Just (SignalSet fp) -> return fp
  withForeignPtr fp $ \p -> do
  c_sigsuspend p
  return ()
  -- ignore the return value; according to the docs it can only ever be
  -- (-1) with errno set to EINTR.
 
foreign import ccall unsafe "sigsuspend"
  c_sigsuspend :: Ptr CSigset -> IO CInt
#endif

foreign import ccall unsafe "__hscore_sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "__hscore_sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "sigpending"
  c_sigpending :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "__hsposix_SIG_BLOCK"   c_SIG_BLOCK   :: CInt
foreign import ccall unsafe "__hsposix_SIG_SETMASK" c_SIG_SETMASK :: CInt
foreign import ccall unsafe "__hsposix_SIG_UNBLOCK" c_SIG_UNBLOCK :: CInt

#endif /* mingw32_TARGET_OS */

