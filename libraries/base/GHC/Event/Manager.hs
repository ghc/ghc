{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , ExistentialQuantification
           , NoImplicitPrelude
           , RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

-- |
-- The event manager supports event notification on fds. Each fd may
-- have multiple callbacks registered, each listening for a different
-- set of events. Registrations may be automatically deactivated after
-- the occurrence of an event ("one-shot mode") or active until
-- explicitly unregistered.
--
-- If an fd has only one-shot registrations then we use one-shot
-- polling if available. Otherwise we use multi-shot polling.

module GHC.Event.Manager
    ( -- * Types
      EventManager

      -- * Creation
    , new
    , newWith
    , newDefaultBackend

      -- * Running
    , finished
    , loop
    , step
    , shutdown
    , release
    , cleanup
    , wakeManager

      -- * State
    , callbackTableVar
    , emControl

      -- * Registering interest in I/O events
    , Lifetime (..)
    , Event
    , evtRead
    , evtWrite
    , FdKey(keyFd)
    , FdData
    , registerFd
    , unregisterFd_
    , unregisterFd
    , closeFd
    , closeFd_
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import Control.Concurrent.MVar (MVar, newMVar, putMVar,
                                tryPutMVar, takeMVar, withMVar,
                                modifyMVar, modifyMVar_)
import Control.Exception (onException,evaluate)
import Data.Bits ((.&.))
import Data.Foldable (forM_,foldl',mapM_)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (maybe)
import Data.OldList (partition)
import GHC.Primitive.Array (Array,indexArray,replicateArrayP)
import GHC.Primitive.SmallArray (SmallArray)
import GHC.Base
import GHC.Conc.Sync (yield,writeTVar,atomically)
import GHC.List (filter)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)
import GHC.Show (Show(..))
import GHC.Event.Control
import GHC.Event.CVar (CVar)
import GHC.Event.IntTable (IntTable)
import GHC.Event.Internal (Event, Lifetime(..), EventLifetime, Timeout(..))
import GHC.Event.Internal (evtClose, evtRead, evtWrite)
import GHC.Event.Unique (Unique, UniqueSource, newSource, newUnique)
import System.Posix.Types (Fd)

import qualified GHC.Event.CVar as CV
import qualified GHC.Event.IntTable as IT
import qualified GHC.Event.Internal as I
import qualified GHC.Event.Backend as I
import qualified GHC.Primitive.Array as PM
import qualified GHC.Primitive.SmallArray as PM

------------------------------------------------------------------------
-- Types

data FdData = FdData {
      fdKey       :: {-# UNPACK #-} !FdKey
    , fdEvents    :: {-# UNPACK #-} !EventLifetime
    , _fdCVar     :: {-# UNPACK #-} !CVar
    }

-- | A file descriptor registration cookie.
data FdKey = FdKey {
      keyFd     :: {-# UNPACK #-} !Fd
    , keyUnique :: {-# UNPACK #-} !Unique
    } deriving ( Eq   -- ^ @since 4.4.0.0
               , Show -- ^ @since 4.4.0.0
               )

data State = Created
           | Running
           | Dying
           | Releasing
           | Finished
             deriving ( Eq   -- ^ @since 4.4.0.0
                      , Show -- ^ @since 4.4.0.0
                      )

-- | The event manager state.
data EventManager = EventManager
    { emBackend      :: {-# UNPACK #-} !I.BackendState
    , emFds          :: {-# UNPACK #-} !(Array (MVar (IntTable FdData)))
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    , emLock         :: {-# UNPACK #-} !(MVar ())
    }

-- This is getting too complicated to attempt. Basically, what I need is
-- for the file descriptor tables to use a plain array instead of a
-- hash table when the total number of file descriptors is small.
-- If a large number of file descriptors are opened, we must migrate
-- to a larger table. The lock-striping makes this difficult. We would
-- need to acquire all the locks at the time same, but this isn't
-- generally possible. It could cause deadlocks. Actually, maybe we
-- don't need to acquire them all at the same time. If we did away
-- with the hash table entirely (migrating from plain arrays to bigger
-- plain arrays), maybe we could do them one-by-one. The locks
-- themselves would be preserved. Only the arrays inside would be
-- changed (replaced with bigger arrays). Although now we need to
-- make sure that concurrent attempts to resize did not interfere
-- with one another. We probably want one big lock for that. It would
-- only be acquired during resizes. This could work.

-- must be power of 2
callbackArraySize :: Int
callbackArraySize = 32

hashFd :: Fd -> Int
hashFd fd = fromIntegral fd .&. (callbackArraySize - 1)
{-# INLINE hashFd #-}

-- See section 3.2 of the Mio paper for an explanation of why we
-- are using lock-striping here.
callbackTableVar :: EventManager -> Fd -> MVar (IntTable FdData)
callbackTableVar mgr fd = indexArray (emFds mgr) (hashFd fd)
{-# INLINE callbackTableVar #-}

haveOneShot :: Bool
{-# INLINE haveOneShot #-}
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
haveOneShot = False
#elif defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
haveOneShot = True
#else
haveOneShot = False
#endif
------------------------------------------------------------------------
-- Creation

handleControlEvent :: EventManager -> Fd -> Event -> IO ()
handleControlEvent mgr fd _evt = do
  msg <- readControlMessage (emControl mgr) fd
  case msg of
    CMsgWakeup      -> return ()
    CMsgDie         -> writeIORef (emState mgr) Finished
    _               -> return ()

newDefaultBackend :: IO I.BackendState
newDefaultBackend = I.new

-- | Create a new event manager.
new :: IO EventManager
new = newWith =<< newDefaultBackend

-- | Create a new 'EventManager' with the given polling backend.
newWith :: I.BackendState -> IO EventManager
newWith be = do
  iofds <- replicateArrayP callbackArraySize
    (newMVar =<< IT.new 8)
  ctrl <- newControl False
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef' state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  lockVar <- newMVar ()
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         , emLock = lockVar
                         }
  registerControlFd mgr (controlReadFd ctrl) evtRead
  registerControlFd mgr (wakeupReadFd ctrl) evtRead
  return mgr

failOnInvalidFile :: String -> Fd -> IO Bool -> IO ()
failOnInvalidFile loc fd m = do
  ok <- m
  when (not ok) $
    let msg = "Failed while attempting to modify registration of file " ++
              show fd ++ " at location " ++ loc
    in errorWithoutStackTrace msg

registerControlFd :: EventManager -> Fd -> Event -> IO ()
registerControlFd mgr fd evs =
  failOnInvalidFile "registerControlFd" fd $
  I.modifyFd (emBackend mgr) fd mempty evs

-- | Asynchronously shuts down the event manager, if running.
shutdown :: EventManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef' (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

-- | Asynchronously tell the thread executing the event
-- manager loop to exit.
release :: EventManager -> IO ()
release EventManager{..} = do
  state <- atomicModifyIORef' emState $ \s -> (Releasing, s)
  when (state == Running) $ sendWakeup emControl

finished :: EventManager -> IO Bool
finished mgr = (== Finished) `liftM` readIORef (emState mgr)

cleanup :: EventManager -> IO ()
cleanup EventManager{..} = do
  writeIORef emState Finished
  void $ tryPutMVar emLock ()
  I.delete emBackend
  closeControl emControl

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop,
-- using 'shutdown'.
--
-- /Note/: This loop can only be run once per 'EventManager', as it
-- closes all of its control resources when it finishes.
loop :: EventManager -> IO ()
loop mgr@EventManager{..} = do
  void $ takeMVar emLock
  state <- atomicModifyIORef' emState $ \s -> case s of
    Created -> (Running, s)
    Releasing -> (Running, s)
    _       -> (s, s)
  case state of
    Created   -> go `onException` cleanup mgr
    Releasing -> go `onException` cleanup mgr
    Dying     -> cleanup mgr
    -- While a poll loop is never forked when the event manager is in the
    -- 'Finished' state, its state could read 'Finished' once the new thread
    -- actually runs.  This is not an error, just an unfortunate race condition
    -- in Thread.restartPollLoop.  See #8235
    Finished  -> return ()
    _         -> do cleanup mgr
                    errorWithoutStackTrace $ "GHC.Event.Manager.loop: state is already " ++
                            show state
 where
  go = do state <- step mgr
          case state of
            Running   -> yield >> go
            Releasing -> putMVar emLock ()
            _         -> cleanup mgr

-- | To make a step, we first do a non-blocking poll, in case
-- there are already events ready to handle. This improves performance
-- because we can make an unsafe foreign C call, thereby avoiding
-- forcing the current Task to release the Capability and forcing a context switch.
-- If the poll fails to find events, we yield, putting the poll loop thread at
-- end of the Haskell run queue. When it comes back around, we do one more
-- non-blocking poll, in case we get lucky and have ready events.
-- If that also returns no events, then we do a blocking poll.
step :: EventManager -> IO State
step mgr@EventManager{..} = do
  waitForIO
  state <- readIORef emState
  state `seq` return state
  where
    waitForIO = do
      n1 <- I.poll emBackend Nothing (onFdEvent mgr)
      when (n1 <= 0) $ do
        yield
        n2 <- I.poll emBackend Nothing (onFdEvent mgr)
        when (n2 <= 0) $ do
          _ <- I.poll emBackend (Just Forever) (onFdEvent mgr)
          return ()

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | Register interest in the given events, without waking the event
-- manager thread.  The 'Bool' return value indicates whether the
-- event manager ought to be woken.
--
-- Note that the event manager is generally implemented in terms of the
-- platform's @select@ or @epoll@ system call, which tend to vary in
-- what sort of fds are permitted. For instance, waiting on regular files
-- is not allowed on many platforms.
--
-- Please ensure that exceptions are masked when this function is called.
-- Everything from GHC.Event.Thread masks exceptions before calling this.
-- We rely on this assumption about the masking state to avoid redundant
-- calls to mask/mask_.
registerFd_ :: EventManager -> CVar -> Fd -> Event -> Lifetime
            -> IO (FdKey, Bool)
registerFd_ mgr@(EventManager{..}) !cv !fd !evs lt = do
  u <- newUnique emUniqueSource
  let fd'  = fromIntegral fd
      reg  = FdKey fd u
      el = I.eventLifetime evs lt
      !fdd = FdData reg el cv
  -- Since the caller of registerFd_ must mask exceptions before calling
  -- it, we do not need the full power of withMVar.
  (modify,ok) <- modifyMVarMaskless (callbackTableVar mgr fd) $ \tbl0 -> do
    (tbl1,oldFdd) <- IT.insertCons fd' fdd tbl0
    let prevEvs :: EventLifetime
        prevEvs = eventsOf oldFdd

        el' :: EventLifetime
        el' = prevEvs `mappend` el
    case I.elLifetime el' of
      -- All registrations want one-shot semantics and this is supported
      OneShot | haveOneShot -> do
        ok <- I.modifyFdOnce emBackend fd (I.elEvent el')
        if ok
          then return (tbl1,(False, True))
          else do
            tbl2 <- IT.reset fd' oldFdd tbl1
            return (tbl2,(False, False))

      -- We don't want or don't support one-shot semantics
      _ -> do
        let modify = prevEvs /= el'
        ok <- if modify
              then let newEvs = I.elEvent el'
                       oldEvs = I.elEvent prevEvs
                   in I.modifyFd emBackend fd oldEvs newEvs
              else return True
        if ok
          then return (tbl1,(modify, True))
          else do
            tbl2 <- IT.reset fd' oldFdd tbl1
            return (tbl2,(False, False))
  -- This simulates behavior of old IO manager:
  -- i.e. just call the callback if the registration fails.
  -- See https://mail.haskell.org/pipermail/ghc-devs/2013-March/000798.html
  -- This only ever happens on the kqueue backend. The other backends
  -- throw an exception when registration fails.
  -- This is dangerous behavior since it can lead to the whole
  -- runtime being blocked by an uninterruptible FFI call. We may
  -- want to consider throwing an exception instead.
  when (not ok) (putReady cv)
  return (reg,modify)

-- | @registerFd mgr cv fd evs lt@ registers interest in the events @evs@
-- on the file descriptor @fd@ for lifetime @lt@. The @cv@ is filled when
-- an event of the requested type occurs.  Returns a cookie that can be
-- handed to 'unregisterFd'.
registerFd :: EventManager -> CVar -> Fd -> Event -> Lifetime -> IO FdKey
registerFd mgr !cv !fd !evs lt = do
  (r, wake) <- registerFd_ mgr cv fd evs lt
  when wake $ wakeManager mgr
  return r

-- This is the same as withMVar except that it doesn't do the
-- mask/restore dance. It should only be used in a context
-- where you know that exceptions are already masked.
modifyMVarMaskless :: MVar a -> (a -> IO (a,b)) -> IO b
{-# INLINE modifyMVarMaskless #-}
modifyMVarMaskless m io = do
  a <- takeMVar m
  (a',b) <- (io a >>= evaluate) `onException` putMVar m a
  putMVar m a'
  return b

{-
    Building GHC with parallel IO manager on Mac freezes when
    compiling the dph libraries in the phase 2. As workaround, we
    don't use oneshot and we wake up an IO manager on Mac every time
    when we register an event.

    For more information, please read:
        http://ghc.haskell.org/trac/ghc/ticket/7651
-}
-- | Wake up the event manager.
wakeManager :: EventManager -> IO ()
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
wakeManager mgr = sendWakeup (emControl mgr)
#elif defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
wakeManager _ = return ()
#else
wakeManager mgr = sendWakeup (emControl mgr)
#endif

eventsOf :: SmallArray FdData -> EventLifetime
eventsOf = foldl' (\acc fdd -> acc <> fdEvents fdd) mempty

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager ought to be woken.
unregisterFd_ :: EventManager -> FdKey -> IO Bool
unregisterFd_ mgr@(EventManager{..}) (FdKey fd u) =
  modifyMVar (callbackTableVar mgr fd) $ \tbl0 -> do
    let dropReg = pure . PM.filterSmallArray ((/= u) . keyUnique . fdKey)
        fd' = fromIntegral fd
    (tbl1, oldFdds, newFdds) <- IT.updateWithM dropReg fd' tbl0
    let oldEls = eventsOf oldFdds
    let newEls = eventsOf newFdds
    let modify = oldEls /= newEls
    when modify $ failOnInvalidFile "unregisterFd_" fd $
      case I.elLifetime newEls of
        OneShot | I.elEvent newEls /= mempty, haveOneShot ->
          I.modifyFdOnce emBackend fd (I.elEvent newEls)
        _ ->
          I.modifyFd emBackend fd (I.elEvent oldEls) (I.elEvent newEls)
    return (tbl1,modify)

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdKey -> IO ()
unregisterFd mgr reg = do
  wake <- unregisterFd_ mgr reg
  when wake $ wakeManager mgr

-- | Close a file descriptor in a race-safe way.
closeFd :: EventManager -> (Fd -> IO ()) -> Fd -> IO ()
closeFd mgr close fd = do
  fds <- modifyMVar (callbackTableVar mgr fd) $ \tbl0 -> do
    (tbl1,fds) <- IT.delete (fromIntegral fd) tbl0
    let oldEls = eventsOf fds
    when (I.elEvent oldEls /= mempty) $ do
      _ <- I.modifyFd (emBackend mgr) fd (I.elEvent oldEls) mempty
      wakeManager mgr
    close fd
    pure (tbl1,fds)
  forM_ fds $ \(FdData _ _ cv) -> CV.close cv

-- | Close a file descriptor in a race-safe way.
-- It assumes the caller will update the callback tables and that the caller
-- holds the callback table lock for the fd. It must hold this lock because
-- this command executes a backend command on the fd.
closeFd_ :: EventManager
         -> IntTable FdData
         -> Fd
         -> IO (IntTable FdData,IO ())
closeFd_ mgr tbl0 fd = do
  (tbl1,fds) <- IT.delete (fromIntegral fd) tbl0
  let oldEls = eventsOf fds
  when (oldEls /= mempty) $ do
    _ <- I.modifyFd (emBackend mgr) fd (I.elEvent oldEls) mempty
    wakeManager mgr
  return (tbl1,mapM_ (\(FdData _ _ cv) -> CV.close cv) fds)


------------------------------------------------------------------------
-- Utilities

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent mgr fd0 evs
  | fd0 == controlReadFd (emControl mgr) || fd0 == wakeupReadFd (emControl mgr) =
    handleControlEvent mgr fd0 evs

  | otherwise = do
    modifyMVar_ (callbackTableVar mgr fd0) $ \tbl0 -> do
        (tbl1,_,_)<- IT.updateWithM fillAndRemoveCVars (fromIntegral fd0) tbl0
        pure tbl1
  where
    -- | Here we look through the list of registrations for the fd of interest
    -- and sort out which match the events that were triggered. We,
    --
    --   1. Re-arm the fd as appropriate
    --   2. Preserve registrations that weren't triggered and multishot
    --      registrations
    --   3. Fill CVars corresponding to registrations for which data
    --      is now available.
    fillAndRemoveCVars :: SmallArray FdData -> IO (SmallArray FdData)
    fillAndRemoveCVars fdds0 = do
      let ((!preservedEls,!allEls),fdds1) = PM.foldlFilterSmallArray'
            ( \(!preservedEls0,!allEls0) fdd ->
              let -- Figure out if this registration has been triggered.
                  isTriggered :: Bool
                  isTriggered = evs `I.eventIs` I.elEvent (fdEvents fdd)
                  -- (triggered, notTriggered) = PM.partitionSmallArray matches fdds

                  -- Is this a multishot registration? If so, we will need
                  -- to retain this registration regardless of whether or
                  -- not it was triggered.
                  isMultishot :: Bool
                  isMultishot = I.elLifetime (fdEvents fdd) == MultiShot
                  
                  -- Should we preserve this registration?
                  isPreserved :: Bool
                  isPreserved = not isTriggered || isMultishot

                  preservedEls1 = if isPreserved
                    then preservedEls0 <> fdEvents fdd
                    else preservedEls0

                  allEls1 = allEls0 <> fdEvents fdd
               in (isPreserved,(preservedEls1,allEls1))
             ) (mempty,mempty) fdds0

      case I.elLifetime allEls of
        -- we previously armed the fd for multiple shots, no need to rearm
        MultiShot | allEls == preservedEls ->
          return ()

        -- either we previously registered for one shot or the
        -- events of interest have changed, we must re-arm
        _ ->
          case I.elLifetime preservedEls of
            OneShot | haveOneShot ->
              -- if there are no saved events and we registered with one-shot
              -- semantics then there is no need to re-arm
              unless (OneShot == I.elLifetime allEls
                      && mempty == I.elEvent preservedEls) $ do
                void $ I.modifyFdOnce (emBackend mgr) fd0 (I.elEvent preservedEls)
            _ ->
              -- we need to re-arm with multi-shot semantics
              void $ I.modifyFd (emBackend mgr) fd0
                                (I.elEvent allEls) (I.elEvent preservedEls)

      return fdds1


-- Notify the reading end of the CVar that the file descriptor they were
-- waiting on is ready for the operation specified earlier.
putReady :: CVar -> IO ()
putReady = CV.match
  (\mv -> putMVar mv True)
  (\tv -> atomically (writeTVar tv CV.Ready))

unless :: Monad m => Bool -> m () -> m ()
unless p = when (not p)
