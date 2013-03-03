{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
           , DeriveDataTypeable
           , StandaloneDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -w -XBangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LWConc.Substrate
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A common interface for lightweight concurrency abstractions.
--
-----------------------------------------------------------------------------


module LwConc.Substrate
(

------------------------------------------------------------------------------
-- PTM
------------------------------------------------------------------------------

  PTM
, unsafeIOToPTM           -- IO a -> PTM a
, atomically              -- PTM a -> IO a
, retry                   -- PTM a

------------------------------------------------------------------------------
-- PVar
------------------------------------------------------------------------------

, PVar
, newPVar                 -- a -> PTM (PVar a)
, newPVarIO               -- a -> IO (PVar a)
, readPVar                -- PVar a -> PTM a
, writePVar               -- PVar a -> a -> PTM ()

------------------------------------------------------------------------------
-- SCont management
------------------------------------------------------------------------------
, SCont
, newSCont                -- IO () -> IO SCont
, getSCont                -- PTM SCont
, getSContIO              -- IO SCont
, getSContId              -- SCont -> PTM Int

------------------------------------------------------------------------------
-- Switch
------------------------------------------------------------------------------

, switch                  -- (SCont -> PTM SCont) -> IO ()
, switchTo                -- SCont -> PTM ()


------------------------------------------------------------------------------
-- Switch
------------------------------------------------------------------------------

, ResumeToken
, newResumeToken          -- PTM ResumeToken
, isResumeTokenValid      -- ResumeToken -> PTM Bool

------------------------------------------------------------------------------
-- SContStatus
------------------------------------------------------------------------------

, SContStatus (..)
, SContSwitchReason (..)
, getSContStatus          -- SCont -> PTM SContStatus
, setSContSwitchReason    -- SCont -> SContSwitchReason -> PTM ()

------------------------------------------------------------------------------
-- Bound SConts
------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
, newBoundSCont           -- IO () -> IO SCont
, isCurrentThreadBound    -- IO Bool
, rtsSupportsBoundThreads -- Bool
#endif

------------------------------------------------------------------------------
-- Upcall actions
------------------------------------------------------------------------------

, setScheduleSContAction  -- SCont -> (SCont -> PTM ()) -> PTM ()
, getScheduleSContAction  -- PTM (SCont -> PTM ())

, setYieldControlAction   -- SCont -> PTM () -> PTM ()
, getYieldControlAction   -- PTM (PTM ())

, setFinalizer            -- SCont -> IO () -> IO ()

------------------------------------------------------------------------------
-- Capability Management
------------------------------------------------------------------------------

, getNumCapabilities       -- IO Int
, getCurrentCapability     -- PTM Int

, setSContCapability       -- SCont -> Int -> IO ()
, getSContCapability       -- SCont -> PTM Int

, sleepCapability          -- PTM a
, scheduleSContOnFreeCap   -- SCont -> IO ()


------------------------------------------------------------------------------
-- Thread-local Storage
------------------------------------------------------------------------------

, setSLS                   -- SCont -> Data.Dynamic -> IO ()
, getSLS                   -- SCont -> PTM Data.Dynamic

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

, throwTo                  -- Exeception e => SCont -> e -> IO ()

, BlockedIndefinitelyOnConcDS(..)
, blockedIndefinitelyOnConcDS

------------------------------------------------------------------------------
-- XXX The following should not be used directly. Only exposed since the RTS
-- cannot find it otherwise. TODO: Hide them. - KC
------------------------------------------------------------------------------

, initSContStatus         -- SContStatus
, scheduleSContActionRts  -- PTM () -> IO ()
, yieldControlActionRts   -- PTM () -> Int -> IO ()

, defaultUpcall           -- IO ()
, defaultExceptionHandler  -- SomeException -> IO ()
) where


import Prelude
import Control.Exception.Base as Exception hiding (throwTo)

#ifdef __GLASGOW_HASKELL__
import GHC.Exception
import GHC.Base
import GHC.Prim
import GHC.IO
import Control.Monad    ( when )
#endif

import System.IO
import GHC.Conc (yield, childHandler, getNumCapabilities)
import Data.Typeable
import Data.Dynamic
import Foreign.StablePtr
import Foreign.C.Types

#include "Typeable.h"

----------------------------------------------------------------------------
-- PTM
----------------------------------------------------------------------------

newtype PTM a = PTM (State# RealWorld -> (# State# RealWorld, a #))


unPTM :: PTM a -> (State# RealWorld -> (# State# RealWorld, a #))
unPTM (PTM a) = a

INSTANCE_TYPEABLE1(PTM,ptmTc,"PTM")

instance  Functor PTM where
   fmap f x = x >>= (return . f)

instance  Monad PTM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenPTM m k
    return x  = returnPTM x
    m >>= k     = bindPTM m k

{-# INLINE bindPTM  #-}
bindPTM :: PTM a -> (a -> PTM b) -> PTM b
bindPTM (PTM m) k = PTM ( \s ->
  case m s of
    (# new_s, a #) -> unPTM (k a) new_s
  )

{-# INLINE thenPTM  #-}
thenPTM :: PTM a -> PTM b -> PTM b
thenPTM (PTM m) k = PTM ( \s ->
  case m s of
    (# new_s, a #) -> unPTM k new_s
  )

{-# INLINE returnPTM  #-}
returnPTM :: a -> PTM a
returnPTM x = PTM (\s -> (# s, x #))

{-# INLINE unsafeIOToPTM  #-}
-- | Unsafely performs IO in the PTM monad.
unsafeIOToPTM :: IO a -> PTM a
unsafeIOToPTM (IO m) = PTM m

-- |Perform a series of PTM actions atomically.
--
-- You cannot use 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'.
-- Any attempt to do so will result in a runtime error.  (Reason: allowing
-- this would effectively allow a transaction inside a transaction, depending
-- on exactly when the thunk is evaluated.)
--
-- However, see 'newPVarIO', which can be called inside 'unsafePerformIO',
-- and which allows top-level PVars to be allocated.

{-# INLINE atomically  #-}
atomically :: PTM a -> IO a
-- atomically = undefined
atomically (PTM c) = IO $ \s10 ->
  atomically# c s10

{-# INLINE retry #-}
retry :: PTM a
retry = PTM $ \s -> retry# s

---------------------------------------------------------------------------------
-- PVar
---------------------------------------------------------------------------------

data PVar a = PVar (TVar# RealWorld a)

INSTANCE_TYPEABLE1(PVar,pvarTc,"PVar")

instance Eq (PVar a) where
  (PVar tvar1#) == (PVar tvar2#) = sameTVar# tvar1# tvar2#

{-# INLINE newPVar  #-}
-- |Create a new PVar holding a value supplied
newPVar :: a -> PTM (PVar a)
newPVar val = PTM $ \s1# ->
    case newTVar# val s1# of
   (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |@IO@ version of 'newPVar'.  This is useful for creating top-level
-- 'PVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
{-# INLINE newPVarIO  #-}
newPVarIO :: a -> IO (PVar a)
newPVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
   (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |Return the current value stored in a PVar
{-# INLINE readPVar #-}
readPVar :: PVar a -> PTM a
readPVar (PVar tvar#) = PTM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a PVar
{-# INLINE writePVar #-}
writePVar :: PVar a -> a -> PTM ()
writePVar (PVar tvar#) val = PTM $ \s1# ->
    case writeTVar# tvar# val s1# of
       s2# -> (# s2#, () #)


---------------------------------------------------------------------------------
-- BlockValid
---------------------------------------------------------------------------------

newtype ResumeToken = ResumeToken (PVar Bool)

{-# INLINE newResumeToken #-}
newResumeToken :: PTM ResumeToken
newResumeToken = do
  t <- newPVar True
  return $ ResumeToken t

{-# INLINE isResumeTokenValid #-}
isResumeTokenValid :: ResumeToken -> PTM Bool
isResumeTokenValid (ResumeToken t) = do
  v <- readPVar t
  return v


---------------------------------------------------------------------------------
-- SContStatus
---------------------------------------------------------------------------------

data SContStatus = SContRunning |
                   SContKilled |
                   SContSwitched SContSwitchReason

data SContSwitchReason = Yielded |
                         BlockedInHaskell ResumeToken |
                         BlockedInRTS |
                         Completed

getIntFromStatus x = case x of
                          SContRunning -> 0
                          SContSwitched Yielded -> 1
                          SContSwitched (BlockedInHaskell _) -> 2
                          SContSwitched BlockedInRTS -> 3
                          SContSwitched Completed -> 4
                          SContKilled -> 5
                          otherwise -> 6

{-# INLINE getSContStatus #-}
getSContStatus :: SCont -> PTM SContStatus
getSContStatus (SCont sc) = do
  st <- PTM $ \s -> case getStatusTVar# sc s of (# s, st #) -> (# s, PVar st #)
  readPVar st

{-# INLINE setSContStatus #-}
setSContStatus :: SCont -> SContStatus -> PTM ()
setSContStatus (SCont sc) status = do
  st <- PTM $ \s -> case getStatusTVar# sc s of (# s, st #) -> (# s, PVar st #)
  writePVar st status

{-# INLINE setSContSwitchReason #-}
setSContSwitchReason :: SCont -> SContSwitchReason -> PTM ()
setSContSwitchReason s reason = do
  setSContStatus s $ SContSwitched reason

{-# INLINE initSContStatus #-}
initSContStatus :: SContStatus
initSContStatus = SContSwitched Yielded


-----------------------------------------------------------------------------------
-- One-shot continuations (SCont)
-----------------------------------------------------------------------------------

data SCont = SCont SCont#

{-# INLINE newSCont #-}
newSCont :: IO () -> IO SCont
newSCont x = do
  IO $ \s ->
   case (newSCont# x s) of (# s1, scont #) -> (# s1, SCont scont #)

{-# INLINE switchTo #-}
switchTo :: SCont -> PTM ()
switchTo targetSCont = do
  -- Set target to Running
  setSContStatus targetSCont SContRunning
  -- Get Int# version of current thread's status to pass to atomicSwitch#
  currentSCont <- getSCont
  status <- getSContStatus currentSCont
  let (I# intStatus) = getIntFromStatus status
  let SCont targetSCont# = targetSCont
  PTM $ \s ->
    case (atomicSwitch# targetSCont# intStatus s) of s1 -> (# s1, () #)

{-# INLINE getSCont #-}
getSCont :: PTM SCont
getSCont = PTM $ \s10 ->
  case getSCont# s10 of
   (# s20, scont #) -> (# s20, SCont scont #)

{-# INLINE getSContIO #-}
getSContIO :: IO SCont
getSContIO = IO $ \s10 ->
  case getSCont# s10 of
   (# s20, scont #) -> (# s20, SCont scont #)

{-# INLINE switch  #-}
switch :: (SCont -> PTM SCont) -> IO ()
switch arg = atomically $ do
  currentSCont <- getSCont
  -- At this point we expect currentSCont status to be Running
  targetSCont <- arg currentSCont
  -- At this point we expect currentSCont status to not be Running
  switchTo targetSCont

{-# INLINE getSContId #-}
getSContId :: SCont -> PTM Int
getSContId (SCont sc) = PTM $ \s ->
  case getSContId# sc s of (# s, i #) -> (# s, (I# i) #)

-----------------------------------------------------------------------------------
-- SCont-local Storage (SLS)
-----------------------------------------------------------------------------------

{-# INLINE setSLS #-}
setSLS :: SCont -> Dynamic -> IO ()
setSLS (SCont sc) v = IO $ \s ->
  case setSLS# sc v s of s -> (# s, () #)

{-# INLINE getSLS #-}
getSLS :: SCont -> PTM Dynamic
getSLS (SCont sc) = PTM $ \s -> getSLS# sc s

-----------------------------------------------------------------------------------
-- yieldControlAction and friends..
-----------------------------------------------------------------------------------

{-# INLINE setYieldControlAction #-}
setYieldControlAction :: SCont -> PTM () -> PTM ()
setYieldControlAction (SCont sc) b = PTM $ \s ->
  case (setYieldControlAction# sc b s) of s -> (# s, () #)

{-# INLINE getYieldControlActionSCont #-}
getYieldControlActionSCont :: SCont -> PTM (PTM ())
getYieldControlActionSCont (SCont sc) =
  PTM $ \s -> getYieldControlAction# sc s

{-# INLINE getYieldControlAction #-}
getYieldControlAction :: PTM (PTM ())
getYieldControlAction = do
  currentSCont <- getSCont
  s <- getYieldControlActionSCont currentSCont
  return s

{-# INLINE yieldControlActionRts #-}
yieldControlActionRts :: SCont -> IO () -- used by RTS
yieldControlActionRts sc = Exception.catch (atomically $ do
  mySC <- getSCont
  setSContSwitchReason mySC Completed
  stat <- getSContStatus sc
  case stat of
      SContRunning -> setSContStatus sc $ SContSwitched BlockedInRTS -- Hasn't been unblocked yet
      SContSwitched Yielded -> return () -- Has been unblocked and put on the run queue
      otherwise -> error "yieldControlAction: Impossible status"
  switch <- getYieldControlActionSCont sc
  switch) (\e -> do {
                      hPutStrLn stderr ("ERROR:" ++ show (e::IOException));
                      error "LwConc.Substrate.yieldControlActionRTS"
                      })

-----------------------------------------------------------------------------------
-- scheduleSContAction and friends..
-----------------------------------------------------------------------------------

{-# INLINE setScheduleSContAction #-}
setScheduleSContAction :: SCont -> (SCont -> PTM ()) -> PTM ()
setScheduleSContAction (SCont sc) r = PTM $ \s ->
  case (setScheduleSContAction# sc r s) of s -> (# s, () #)

{-# INLINE getScheduleSContActionSCont #-}
getScheduleSContActionSCont :: SCont -> PTM(SCont -> PTM ())
getScheduleSContActionSCont (SCont sc) =
  PTM $ \s -> getScheduleSContAction# sc s

{-# INLINE getScheduleSContAction #-}
getScheduleSContAction :: PTM (SCont -> PTM ())
getScheduleSContAction = do
  currentSCont <- getSCont
  u <- getScheduleSContActionSCont currentSCont
  return u

{-# INLINE scheduleSContActionRts #-}
scheduleSContActionRts :: SCont -> IO () -- used by RTS
scheduleSContActionRts sc = Exception.catch (atomically $ do
  stat <- getSContStatus sc
  case stat of
    SContSwitched (BlockedInHaskell (ResumeToken t)) -> writePVar t False
    otherwise -> return ()
  setSContStatus sc $ SContSwitched Yielded
  unblock <- getScheduleSContActionSCont sc
  unblock sc) (\e -> do {
                      hPutStrLn stderr ("ERROR:" ++ show (e::IOException));
                      error "LwConc.Substrate.scheduleSContActionRTS"
                      })


-----------------------------------------------------------------------------------
-- Misc upcalls
-----------------------------------------------------------------------------------

{-# INLINE setFinalizer #-}
setFinalizer :: SCont -> IO () -> IO ()
setFinalizer (SCont sc) b = IO $ \s ->
  case (setFinalizer# sc b s) of s -> (# s, () #)

defaultUpcall :: IO ()
defaultUpcall = IO $ \s-> (# defaultUpcallError# s, () #)

defaultExceptionHandler :: Exception.SomeException -> IO ()
defaultExceptionHandler _ = atomically $ do
  s <- getSCont
  setSContStatus s SContKilled
  yca <- getYieldControlAction
  yca


----------------------------------------------------------------------------
-- Bound threads
----------------------------------------------------------------------------

-- | 'True' if bound threads are supported.
-- If @rtsSupportsBoundThreads@ is 'False', 'isCurrentThreadBound'
-- will always return 'False' and both 'forkOS' and 'runInBoundThread' will
-- fail.
foreign import ccall rtsSupportsBoundThreads :: Bool

-- | Returns 'True' if the calling thread is /bound/, that is, if it is
-- safe to use foreign libraries that rely on thread-local state from the
-- calling thread.
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = IO $ \ s# ->
    case isCurrentThreadBound# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

isThreadBound :: SCont -> IO Bool
isThreadBound (SCont sc) = IO $ \ s# ->
    case isThreadBound# sc s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)


isThreadBoundPTM :: SCont -> PTM Bool
isThreadBoundPTM (SCont sc) = PTM $ \ s# ->
    case isThreadBound# sc s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)



failNonThreaded :: IO a
failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"

foreign import ccall forkOS_createThreadForSCont
    :: StablePtr (SCont) -> IO CInt

newBoundSCont :: IO () -> IO SCont
newBoundSCont action0
  | rtsSupportsBoundThreads = do
        b <- Exception.getMaskingState
        callingSCont <- getSContIO
        let
            -- async exceptions are masked in the child if they are masked
            -- in the parent, as for forkIO (see #1048). forkOS_createThread
            -- creates a thread with exceptions masked by default.
            action1 = case b of
                        Unmasked -> unsafeUnmask action0
                        MaskedInterruptible -> action0
                        MaskedUninterruptible -> uninterruptibleMask_ action0
            action2 = switchback >> action1
                      where switchback = atomically $ do {
                                            mySC <- getSCont;
                                            setSContSwitchReason mySC Yielded;
                                            switchTo callingSCont
                                         }
            action_plus = Exception.catch action2 childHandler
        s <- newSCont action_plus
        entry <- newStablePtr s
        err <- forkOS_createThreadForSCont entry
        when (err /= 0) $ fail "Cannot create OS thread."
        -- Wait for initialization
        let wait = do {
          r <- isThreadBound s;
          if r
             then
               return ()
             else do {
               yield;
               wait
               }
        }
        wait
        -- At this point we know that a new OS thread and a bound task have been
        -- created and the bound SCont's TSO structure has been marked as bound.
        -- But we need to make sure that new bound task has entered the schedule
        -- () loop and has yielded the capability. Hence, we switch to and
        -- switch back (see action2 above) from the new bound task.
        atomically $ do {
          mySC <- getSCont;
          setSContSwitchReason mySC Yielded;
          switchTo s
        }
        -- We are back.
        freeStablePtr entry
        return s
  | otherwise = failNonThreaded


----------------------------------------------------------------------------
-- Spinning up more schedulers (Experimental)

-- Given a bound thread, assigns it a free capability. If there are no free
-- capabilities, this call will never return!
----------------------------------------------------------------------------

scheduleSContOnFreeCap :: SCont -> IO ()
scheduleSContOnFreeCap (SCont s) = IO $
  \st -> case scheduleThreadOnFreeCap# s st of st -> (# st, () #)

------------------------------------------------------------------------------
-- Capability Management
------------------------------------------------------------------------------

getCurrentCapability :: PTM Int
getCurrentCapability = PTM $
  \s -> case getCurrentCapability# s of
             (# s, n #) -> (# s, I# n #)

getCurrentCapabilityIO :: IO Int
getCurrentCapabilityIO = IO $
  \s -> case getCurrentCapability# s of
             (# s, n #) -> (# s, I# n #)

-- Returns the capability the given scont is bound to
getSContCapability :: SCont -> PTM Int
getSContCapability (SCont sc) = PTM $
  \s -> case getSContCapability# sc s of
             (# s, n #) -> (# s, I# n #)

-- Returns the capability the given scont is bound to
getSContCapabilityIO :: SCont -> IO Int
getSContCapabilityIO (SCont sc) = IO $
  \s -> case getSContCapability# sc s of
             (# s, n #) -> (# s, I# n #)


-- We must own the capability (i.e, scont->cap == MyCapability ()). Otherwise,
-- will throw a runtime error.
setSContCapability :: SCont -> Int -> IO ()
setSContCapability (SCont sc) (I# i) = do
  cc <- getCurrentCapabilityIO
  scc <- getSContCapabilityIO $ SCont sc
  if cc == scc
    then IO $ \s -> case setSContCapability# sc i s of s -> (# s, () #)
    else error "setSContCapability: SCont must belong to the current capability"

sleepCapability :: PTM a
sleepCapability = PTM $ \s -> sleepCapability# s

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------


throwTo :: Exception e => SCont -> e -> IO ()
throwTo (SCont sc) ex = IO $ \s ->
  case (killSCont# sc (toException ex) s) of s -> (# s, () #)

data BlockedIndefinitelyOnConcDS = BlockedIndefinitelyOnConcDS
    deriving Typeable

instance Exception BlockedIndefinitelyOnConcDS

instance Show BlockedIndefinitelyOnConcDS where
    showsPrec _ BlockedIndefinitelyOnConcDS = showString "thread blocked indefinitely in an STM transaction"

blockedIndefinitelyOnConcDS :: SomeException -- for the RTS
blockedIndefinitelyOnConcDS = toException BlockedIndefinitelyOnConcDS
