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

#include "HsBaseConfig.h"

module System.Posix.Signals (
#ifndef mingw32_HOST_OS
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
#if CONST_SIGPOLL != -1
  pollableEvent, sigPOLL,
#endif
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

#ifdef __GLASGOW_HASKELL__
  -- * Handling signals
  Handler(..),
  installHandler,
#endif

  -- * Signal sets
  SignalSet,
  emptySignalSet, fullSignalSet, 
  addSignal, deleteSignal, inSignalSet,

  -- * The process signal mask
  getSignalMask, setSignalMask, blockSignals, unblockSignals,

  -- * The alarm timer
  scheduleAlarm,

  -- * Waiting for signals
  getPendingSignals,
#ifndef cygwin32_HOST_OS
  awaitSignal,
#endif

#ifdef __GLASGOW_HASKELL__
  -- * The @NOCLDSTOP@ flag
  setStoppedChildFlag, queryStoppedChildFlag,
#endif

  -- MISSING FUNCTIONALITY:
  -- sigaction(), (inc. the sigaction structure + flags etc.)
  -- the siginfo structure
  -- sigaltstack()
  -- sighold, sigignore, sigpause, sigrelse, sigset
  -- siginterrupt
#endif
  ) where

import Prelude -- necessary to get dependencies right

import Foreign
import Foreign.C
import System.IO.Unsafe
import System.Posix.Types
import System.Posix.Internals

#ifndef mingw32_HOST_OS
-- WHOLE FILE...

#ifdef __GLASGOW_HASKELL__
#include "Signals.h"
import GHC.Conc	( ensureIOManagerIsRunning )
#endif

-- -----------------------------------------------------------------------------
-- Specific signals

type Signal = CInt

nullSignal :: Signal
nullSignal = 0

sigABRT   :: CInt
sigABRT   = CONST_SIGABRT
sigALRM   :: CInt
sigALRM   = CONST_SIGALRM
sigBUS    :: CInt
sigBUS    = CONST_SIGBUS
sigCHLD   :: CInt
sigCHLD   = CONST_SIGCHLD
sigCONT   :: CInt
sigCONT   = CONST_SIGCONT
sigFPE    :: CInt
sigFPE    = CONST_SIGFPE
sigHUP    :: CInt
sigHUP    = CONST_SIGHUP
sigILL    :: CInt
sigILL    = CONST_SIGILL
sigINT    :: CInt
sigINT    = CONST_SIGINT
sigKILL   :: CInt
sigKILL   = CONST_SIGKILL
sigPIPE   :: CInt
sigPIPE   = CONST_SIGPIPE
sigQUIT   :: CInt
sigQUIT   = CONST_SIGQUIT
sigSEGV   :: CInt
sigSEGV   = CONST_SIGSEGV
sigSTOP   :: CInt
sigSTOP   = CONST_SIGSTOP
sigTERM   :: CInt
sigTERM   = CONST_SIGTERM
sigTSTP   :: CInt
sigTSTP   = CONST_SIGTSTP
sigTTIN   :: CInt
sigTTIN   = CONST_SIGTTIN
sigTTOU   :: CInt
sigTTOU   = CONST_SIGTTOU
sigUSR1   :: CInt
sigUSR1   = CONST_SIGUSR1
sigUSR2   :: CInt
sigUSR2   = CONST_SIGUSR2
sigPOLL   :: CInt
sigPOLL   = CONST_SIGPOLL
sigPROF   :: CInt
sigPROF   = CONST_SIGPROF
sigSYS    :: CInt
sigSYS    = CONST_SIGSYS
sigTRAP   :: CInt
sigTRAP   = CONST_SIGTRAP
sigURG    :: CInt
sigURG    = CONST_SIGURG
sigVTALRM :: CInt
sigVTALRM = CONST_SIGVTALRM
sigXCPU   :: CInt
sigXCPU   = CONST_SIGXCPU
sigXFSZ   :: CInt
sigXFSZ   = CONST_SIGXFSZ

internalAbort ::Signal
internalAbort = sigABRT

realTimeAlarm :: Signal
realTimeAlarm = sigALRM

busError :: Signal
busError = sigBUS

processStatusChanged :: Signal
processStatusChanged = sigCHLD

continueProcess :: Signal
continueProcess = sigCONT

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

#if CONST_SIGPOLL != -1
pollableEvent :: Signal
pollableEvent = sigPOLL
#endif

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

-- | @signalProcess int pid@ calls @kill@ to signal process @pid@ 
--   with interrupt signal @int@.
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess sig pid 
 = throwErrnoIfMinus1_ "signalProcess" (c_kill (fromIntegral pid) sig)

foreign import ccall unsafe "kill"
  c_kill :: CPid -> CInt -> IO CInt


-- | @signalProcessGroup int pgid@ calls @kill@ to signal 
--  all processes in group @pgid@ with interrupt signal @int@.
signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup sig pgid 
  = throwErrnoIfMinus1_ "signalProcessGroup" (c_killpg (fromIntegral pgid) sig)

foreign import ccall unsafe "killpg"
  c_killpg :: CInt -> CInt -> IO CInt

-- | @raiseSignal int@ calls @kill@ to signal the current process
--   with interrupt signal @int@. 
raiseSignal :: Signal -> IO ()
raiseSignal sig = throwErrnoIfMinus1_ "raiseSignal" (c_raise sig)

#if defined(__GLASGOW_HASKELL__) && (defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS))
foreign import ccall unsafe "genericRaise"
  c_raise :: CInt -> IO CInt
#else
foreign import ccall unsafe "raise"
  c_raise :: CInt -> IO CInt
#endif

#ifdef __GLASGOW_HASKELL__
data Handler = Default
             | Ignore
	     -- not yet: | Hold 
             | Catch (IO ())
             | CatchOnce (IO ())

-- | @installHandler int handler iset@ calls @sigaction@ to install an
--   interrupt handler for signal @int@.  If @handler@ is @Default@,
--   @SIG_DFL@ is installed; if @handler@ is @Ignore@, @SIG_IGN@ is
--   installed; if @handler@ is @Catch action@, a handler is installed
--   which will invoke @action@ in a new thread when (or shortly after) the
--   signal is received.
--   If @iset@ is @Just s@, then the @sa_mask@ of the @sigaction@ structure
--   is set to @s@; otherwise it is cleared.  The previously installed
--   signal handler for @int@ is returned
installHandler :: Signal
               -> Handler
               -> Maybe SignalSet	-- ^ other signals to block
               -> IO Handler		-- ^ old handler

#ifdef __PARALLEL_HASKELL__
installHandler = 
  error "installHandler: not available for Parallel Haskell"
#else

installHandler int handler maybe_mask = do
    ensureIOManagerIsRunning  -- for the threaded RTS
    case maybe_mask of
	Nothing -> install' nullPtr
        Just (SignalSet x) -> withForeignPtr x $ install' 
  where 
    install' mask = 
      alloca $ \p_sp -> do

      rc <- case handler of
      	      Default      -> stg_sig_install int STG_SIG_DFL p_sp mask
      	      Ignore       -> stg_sig_install int STG_SIG_IGN p_sp mask
      	      Catch m      -> hinstall m p_sp mask int STG_SIG_HAN
      	      CatchOnce m  -> hinstall m p_sp mask int STG_SIG_RST

      case rc of
	STG_SIG_DFL -> return Default
	STG_SIG_IGN -> return Ignore
	STG_SIG_ERR -> throwErrno "installHandler"
	STG_SIG_HAN -> do
        	m <- peekHandler p_sp
		return (Catch m)
	STG_SIG_RST -> do
        	m <- peekHandler p_sp
		return (CatchOnce m)
	_other ->
	   error "internal error: System.Posix.Signals.installHandler"

    hinstall m p_sp mask int reset = do
      sptr <- newStablePtr m
      poke p_sp sptr
      stg_sig_install int reset p_sp mask

    peekHandler p_sp = do
      osptr <- peek p_sp
      deRefStablePtr osptr

foreign import ccall unsafe
  stg_sig_install
	:: CInt				-- sig no.
	-> CInt				-- action code (STG_SIG_HAN etc.)
	-> Ptr (StablePtr (IO ()))	-- (in, out) Haskell handler
	-> Ptr CSigset			-- (in, out) blocked
	-> IO CInt			-- (ret) action code

#endif /* !__PARALLEL_HASKELL__ */
#endif /* __GLASGOW_HASKELL__ */

-- -----------------------------------------------------------------------------
-- Alarms

-- | @scheduleAlarm i@ calls @alarm@ to schedule a real time
--   alarm at least @i@ seconds in the future.
scheduleAlarm :: Int -> IO Int
scheduleAlarm secs = do
   r <- c_alarm (fromIntegral secs)
   return (fromIntegral r)

foreign import ccall unsafe "alarm"
  c_alarm :: CUInt -> IO CUInt

#ifdef __GLASGOW_HASKELL__
-- -----------------------------------------------------------------------------
-- The NOCLDSTOP flag

foreign import ccall "&nocldstop" nocldstop :: Ptr Int

-- | Tells the system whether or not to set the @SA_NOCLDSTOP@ flag when
-- installing new signal handlers.
setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag b = do
    rc <- peek nocldstop
    poke nocldstop $ fromEnum (not b) 
    return (rc == (0::Int))

-- | Queries the current state of the stopped child flag.
queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = do
    rc <- peek nocldstop
    return (rc == (0::Int))
#endif /* __GLASGOW_HASKELL__ */

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

-- | @getSignalMask@ calls @sigprocmask@ to determine the
--   set of interrupts which are currently being blocked.
getSignalMask :: IO SignalSet
getSignalMask = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getSignalMask" (c_sigprocmask 0 nullPtr p)
  return (SignalSet fp)
   
sigProcMask :: String -> CInt -> SignalSet -> IO ()
sigProcMask fn how (SignalSet set) =
  withForeignPtr set $ \p_set ->
    throwErrnoIfMinus1_ fn (c_sigprocmask how p_set nullPtr)

-- | @setSignalMask mask@ calls @sigprocmask@ with
--   @SIG_SETMASK@ to block all interrupts in @mask@.
setSignalMask :: SignalSet -> IO ()
setSignalMask set = sigProcMask "setSignalMask" (CONST_SIG_SETMASK :: CInt) set

-- | @blockSignals mask@ calls @sigprocmask@ with
--   @SIG_BLOCK@ to add all interrupts in @mask@ to the
--  set of blocked interrupts.
blockSignals :: SignalSet -> IO ()
blockSignals set = sigProcMask "blockSignals" (CONST_SIG_BLOCK :: CInt) set

-- | @unblockSignals mask@ calls @sigprocmask@ with
--   @SIG_UNBLOCK@ to remove all interrupts in @mask@ from the
--   set of blocked interrupts. 
unblockSignals :: SignalSet -> IO ()
unblockSignals set = sigProcMask "unblockSignals" (CONST_SIG_UNBLOCK :: CInt) set

-- | @getPendingSignals@ calls @sigpending@ to obtain
--   the set of interrupts which have been received but are currently blocked.
getPendingSignals :: IO SignalSet
getPendingSignals = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p -> 
   throwErrnoIfMinus1_ "getPendingSignals" (c_sigpending p)
  return (SignalSet fp)

#ifndef cygwin32_HOST_OS

-- | @awaitSignal iset@ suspends execution until an interrupt is received.
-- If @iset@ is @Just s@, @awaitSignal@ calls @sigsuspend@, installing
-- @s@ as the new signal mask before suspending execution; otherwise, it
-- calls @pause@.  @awaitSignal@ returns on receipt of a signal.  If you
-- have installed any signal handlers with @installHandler@, it may be
-- wise to call @yield@ directly after @awaitSignal@ to ensure that the
-- signal handler runs as promptly as possible.
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

#ifdef __HUGS__
foreign import ccall unsafe "sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt
#else
foreign import ccall unsafe "__hscore_sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "__hscore_sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt
#endif /* __HUGS__ */

foreign import ccall unsafe "sigpending"
  c_sigpending :: Ptr CSigset -> IO CInt

#endif /* mingw32_HOST_OS */

