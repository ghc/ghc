{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.Windows
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- WinIO Windows event manager.
--
-------------------------------------------------------------------------------

module GHC.Event.Windows (
    -- * Manager
    Manager,
    getSystemManager,
    interruptSystemManager,
    wakeupIOManager,
    processRemoteCompletion,

    -- * Overlapped I/O
    associateHandle,
    associateHandle',
    withOverlapped,
    withOverlappedEx,
    StartCallback,
    StartIOCallback,
    CbResult(..),
    CompletionCallback,
    LPOVERLAPPED,

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,

    -- * Utilities
    withException,
    ioSuccess,
    ioFailed,
    getLastError,

    -- * I/O Result type
    IOResult(..),

    -- * I/O Event notifications
    HandleData (..), -- seal for release
    HandleKey (handleValue),
    registerHandle,
    unregisterHandle,

    -- * Console events
    module GHC.Event.Windows.ConsoleEvent
) where

-- define DEBUG 1

##include "windows_cconv.h"
#include <windows.h>
#include <ntstatus.h>
#include <Rts.h>
#include "winio_structs.h"

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (LPOVERLAPPED, OVERLAPPED_ENTRY(..))
import GHC.Event.Windows.ManagedThreadPool
import GHC.Event.Internal.Types
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.PSQ            as Q
import qualified GHC.Event.IntTable       as IT
import qualified GHC.Event.Internal as I

import {-# SOURCE #-} Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.IORef
import Data.Foldable (mapM_, length, forM_)
import Data.Maybe
import Data.Word
import Data.Semigroup.Internal (stimesMonoid)
import Data.OldList (deleteBy)
import Foreign
import Foreign.ForeignPtr.Unsafe
import qualified GHC.Event.Array    as A
import GHC.Base
import GHC.Conc.Sync (forkIO, showThreadId,
                      ThreadId(..), ThreadStatus(..),
                      threadStatus, sharedCAF)
import GHC.Event.Unique
import GHC.Event.TimeOut
import GHC.Event.Windows.ConsoleEvent
import GHC.IOPort
import GHC.Num
import GHC.Real
import GHC.Windows
import GHC.List (null)
import GHC.Ptr
import System.IO.Unsafe     (unsafePerformIO)
import Text.Show
import GHC.RTS.Flags

-- if defined(DEBUG)
#if 1
import Foreign.C
import System.Posix.Internals (c_write)
import GHC.Conc.Sync (myThreadId)
#endif

import qualified GHC.Windows as Win32

-- Note [WINIO Manager design]
-- This file contains the Windows I//O manager. Windows's IO subsystem is by
-- design fully asynchronous, however there are multiple ways and interfaces
-- to the async methods.
--
-- The chosen Async interface for this implementation is using Completion Ports
-- See also Note [Completion Ports]. The I/O manager uses a new interface added
-- in Windows Vista called `GetQueuedCompletionStatusEx` which allows us to
-- service multiple requests in one go.
--
-- See https://docs.microsoft.com/en-us/windows-hardware/drivers/kernel/overview-of-the-windows-i-o-model
-- and https://www.microsoftpressstore.com/articles/article.aspx?p=2201309&seqNum=3
--
-- In order to understand this file, here is what you should know:
-- We're using relatively new APIs that allow us to service multiple requests at
-- the same time using one OS thread.  This happens using so called Completion
-- ports.  All I/O actions get associated with one and the same completion port.
--
-- The I/O manager itself has two mode of operation:
-- 1) Threaded: We have N dedicated OS threads in the Haskell world that service
--    completion requests.  Everything is Handled 100% in view of the runtime.
--    Whenever the OS has completions that need to be serviced it wakes up one
--    one of the OS threads that are blocked in GetQueuedCompletionStatusEx and
--    lets it proceed  with the list of completions that are finished. If more
--    completions finish before the first list is done being processed then
--    another thread is woken up.  These threads are associated with the I/O
--    manager through the completion port.  If it blocks for any reason the
--    I/O manager will wake up another thread from the pool to finish processing
--    the remaining entries.  This worker threads must be able to handle the
--    fact that something else has finished the remainder of their queue or must
--    have a guarantee to never block.  In this implementation we strive to
--    never block.   This is achieved by not having the worker threads call out
--    to any user code, and to have the IOPort synchronization primitive never
--    block.   This means if the port is full the message is lost, however we
--    have an invariant that the port can never be full and have a waiting
--    receiver.  As such, dropping the message does not change anything as there
--    will never be anyone to receive it. e.g. it is an impossible situation to
--    land in.
-- 2) Non-threaded: We don't have any dedicated Haskell threads at servicing
--    I/O Requests. Instead we have an OS thread inside the RTS that gets
--    notified of new requests and does the servicing.  When a request completes
--    a Haskell thread is scheduled to run to finish off the processing of any
--    completed requests. See Note [Non-Threaded WINIO design].
--
-- These two modes of operations share the majority of the code and so they both
-- support the same operations and fixing one will fix the other. (See the step
-- function.)
-- Unlike MIO, we don't threat network I/O any differently than file I/O. Hence
-- any network specific code is now only in the network package.
--
-- Note [Threaded WINIO design]
-- The threaded WiNIO is designed around a simple blocking call that's called in
-- a service loop in a dedicated thread: `GetQueuedCompletionStatusEx`.
-- as such the loop is reasonably simple.  We're either servicing finished
-- requests or blocking in `getQueuedCompletionStatusEx` waiting for new
-- requests to arrive.
--
-- Each time a Handle is made three important things happen that affect the I/O
-- manager design:
-- 1) Files are opened with the `FILE_FLAG_OVERLAPPED` flag, which instructs the
--    OS that we will be doing purely asynchronous requests. See
--    `GHC.IO.Windows.Handle.openFile`.  They are also opened with
--    `FILE_FLAG_SEQUENTIAL_SCAN` to indicate to the OS that we want to optimize
--    the access of the file for sequential access. (e.g. equivalent to MADVISE)
-- 2) The created handle is associated with the I/O manager's completion port.
--    This allows the I/O manager to be able to service I/O events from this
--    handle.  See `associateHandle`.
-- 3) File handles are additionally modified with two optimization flags:
--
--    FILE_SKIP_COMPLETION_PORT_ON_SUCCESS: If the request can be serviced
--    immediately, then do not queue the IRP (IO Request Packet) into the I/O
--    manager waiting for us to service it later.  Instead service it
--    immediately in the same call.  This is beneficial for two reasons:
--    1) We don't have to block in the Haskell RTS.
--    2) We save a bunch of work in the OS's I/O subsystem.
--    The downside is though that we have to do a bunch of work to handle these
--    cases.  This is abstracted away from the user by the `withOverlapped`
--    function.
--    This together with the buffering strategy mentioned above means we
--    actually skip the I/O manager on quite a lot of I/O requests due to the
--    value being in the cache.  Because of the Lazy I/O in Haskell, the time
--    to read and decode the buffer of bytes is usually longer than the OS needs
--    to read the next chunk, so we hit the FAST_IO IRP quite often.
--
--    FILE_SKIP_SET_EVENT_ON_HANDLE: Since we will not be using an event object
--    to monitor asynchronous completions, don't bother updating or checking for
--    one.  This saves some precious cycles, especially on operations with very
--    high number of I/O operations (e.g. servers.)
--
-- So what does servicing a request actually mean.  As mentioned before the
-- I/O manager will be blocked or servicing a request. In reality it doesn't
-- always block till an I/O request has completed.  In cases where we have event
-- timers, we block till the next timer's timeout.  This allows us to also
-- service timers in the same loop.  The side effect of this is that we will
-- exit the I/O wait sometimes without any completions.  Not really a problem
-- but it's an important design decision.
--
-- Every time we wait, we give a pre-allocated buffer of `n`
-- `OVERLAPPED_ENTRIES` to the OS.  This means that in a single call we can
-- service up to `n` I/O requests at a time.  The size of `n` is not fixed,
-- anytime we dequeue `n` I/O requests in a single operation we double the
-- buffer size, allowing the I/O manager to be able to scale up depending
-- on the workload.  This buffer is kept alive throughout the lifetime of the
-- program and is never freed until the I/O manager is shutting down.
--
-- One very important property of the I/O subsystem is that each I/O request
-- now requires an `OVERLAPPED` structure be given to the I/O manager.  See
-- `withOverlappedEx`.  This buffer is used by the OS to fill in various state
-- information by the OS. Throughout the duration of I/O call, this buffer MUST
-- remain live.  The address is pinned by the kernel, which means that the
-- pointer must remain accessible until `GetQueuedCompletionStatusEx` returns
-- the completion associated with the handle and not just until the call to what
-- ever I/O operation was used to initialize the I/O request returns.
-- The only exception to this is when the request has hit the FAST_IO path, in
-- which case it has skipped the I/O queue and so can be freed immediately after
-- reading the results from it.
--
-- To prevent having to lookup the Haskell payload in a shared state after the
-- request completes we attach it as part of the I/O request by extending the
-- `OVERLAPPED` structure.  Instead of passing an `OVERLAPPED` structure to the
-- Windows API calls we instead pass a `HASKELL_OVERLAPPED` struct which has
-- as the first element an `OVERLAPPED structure.  This means when a request is
-- done all we need to do is cast the pointer back to `HASKELL_OVERLAPPED` and
-- read the accompanying data.  This also means we don't have a global lock and
-- so can scale much easier.

-- ---------------------------------------------------------------------------
-- I/O manager resume/suspend code

{-# NOINLINE ioManagerThread #-}
ioManagerThread :: MVar (Maybe ThreadId)
ioManagerThread = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetGHCConcWindowsIOManagerThreadStore

foreign import ccall unsafe "getOrSetGHCConcWindowsIOManagerThreadStore"
  getOrSetGHCConcWindowsIOManagerThreadStore :: Ptr a -> IO (Ptr a)

-- ---------------------------------------------------------------------------
-- Non-threaded I/O manager callback hooks. See `ASyncWinIO.c`

foreign import ccall safe "registerNewIOCPHandle"
  registerNewIOCPHandle :: FFI.IOCP -> IO ()

foreign import ccall safe "registerAlertableWait"
  registerAlertableWait :: FFI.IOCP -> DWORD -> Word64 -> IO ()

foreign import ccall safe "getOverlappedEntries"
  getOverlappedEntries :: Ptr DWORD -> IO (Ptr OVERLAPPED_ENTRY)

foreign import ccall safe "servicedIOEntries"
  servicedIOEntries :: Word64 -> IO ()

foreign import ccall safe "completeSynchronousRequest"
  completeSynchronousRequest :: IO ()

------------------------------------------------------------------------
-- Manager structures

-- | Callback type that will be called when an I/O operation completes.
type IOCallback = CompletionCallback ()

-- | Wrap the IOCallback type into a FunPtr.
foreign import ccall "wrapper"
  wrapIOCallback :: IOCallback -> IO (FunPtr IOCallback)

-- | Unwrap a FunPtr IOCallback to a normal Haskell function.
foreign import ccall "dynamic"
  mkIOCallback :: FunPtr IOCallback -> IOCallback

-- | Structure that the I/O managed uses to to associate callbacks with
-- it's additional payload such as it's OVERLAPPED structure and Win32 handle
-- etc.  Must be kept in sync with that in `winio_structs.h` or horrible things
-- happen.
data CompletionData = CompletionData { cdHandle   :: !HANDLE
                                     , cdCallback :: !IOCallback
                                     }

instance Storable CompletionData where
    sizeOf _    = #{size CompletionData}
    alignment _ = #{alignment CompletionData}

    peek ptr = do
      cdHandle   <- #{peek CompletionData, cdHandle} ptr
      cdCallback <- mkIOCallback `fmap` #{peek CompletionData, cdCallback} ptr
      let !cd = CompletionData{..}
      return cd

    poke ptr CompletionData{..} = do
      #{poke CompletionData, cdHandle} ptr cdHandle
      cb <- wrapIOCallback cdCallback
      #{poke CompletionData, cdCallback} ptr cb

-- | Pointer offset in bytes to the location of hoData in HASKELL_OVERLAPPPED
cdOffset :: Int
cdOffset = #{const __builtin_offsetof (HASKELL_OVERLAPPED, hoData)}

-- | Terminator symbol for IOCP request
nullReq :: Ptr (Ptr a)
nullReq = castPtr $ unsafePerformIO $ new $ (nullPtr :: Ptr ())

-- I don't expect a lot of events, so a simple linked lists should be enough.
type EventElements = [(Event, HandleData)]
data EventData = EventData { evtTopLevel :: !Event, evtElems :: !EventElements }

instance Monoid EventData where
  mempty  = EventData evtNothing []
  mappend = (<>)

instance Semigroup EventData where
  (<>)   = \a b -> EventData (evtTopLevel a <> evtTopLevel b)
                             (evtElems a ++ evtElems b)
  stimes = stimesMonoid

data IOResult a
  = IOSuccess { ioValue :: a }
  | IOFailed  { ioErrCode :: Maybe Int }

-- | The state object for the I/O manager.  This structure is available for both
-- the threaded and the non-threaded RTS.
data Manager = Manager
    { mgrIOCP         :: {-# UNPACK #-} !FFI.IOCP
    , mgrClock        ::                !Clock
    , mgrUniqueSource :: {-# UNPACK #-} !UniqueSource
    , mgrTimeouts     :: {-# UNPACK #-} !(IORef TimeoutQueue)
    , mgrEvntHandlers :: {-# UNPACK #-}
                         !(MVar (IT.IntTable EventData))
    , mgrOverlappedEntries
                      :: {-#UNPACK #-} !(A.Array OVERLAPPED_ENTRY)
    , mgrThreadPool   :: Maybe ThreadPool
    }

-- | Create a new I/O manager. In the Threaded I/O manager this call doesn't
-- have any side effects, but in the Non-Threaded I/O manager the newly
-- created IOCP handle will be registered with the RTS.  Users should never
-- call this.
--
-- NOTE: This needs to finish without making any calls to anything requiring the
-- I/O manager otherwise we'll get into some weird synchronization issues.
-- Essentially this means avoid using long running operations here.
newManager :: IO Manager
newManager = do
    debugIO "Starting io-manager..."
    mgrIOCP         <- FFI.newIOCP
    when (not threaded) $
      registerNewIOCPHandle mgrIOCP
    debugIO $ "iocp: " ++ show mgrIOCP
    mgrClock             <- getClock
    mgrUniqueSource      <- newSource
    mgrTimeouts          <- newIORef Q.empty
    mgrOverlappedEntries <- A.new 64
    mgrEvntHandlers      <- newMVar =<< IT.new callbackArraySize
    let mgrThreadPool    = Nothing

    let !mgr = Manager{..}
    return mgr

{-# INLINE startIOManagerThread #-}
-- | Starts a new I/O manager thread.
-- For the threaded runtime it creates a pool of OS threads which stays alive
-- until they are instructed to die. For the non-threaded runtime we have a
-- single worker thread in the C runtime.
startIOManagerThread :: IO () -> IO ()
startIOManagerThread loop = do
  modifyMVar_ ioManagerThread $ \old -> do
    let create = do debugIO "spawning worker threads.."
                    t <- if threaded
                            then forkOS loop
                            else forkIO loop
                    setStatus WinIORunning
                    debugIO $ "created io-manager threads."
                    return (Just t)
    case old of
      Nothing -> create
      Just t  -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> do status <- getStatus
                               case status of
                                WinIOBlocked  -> do
                                  c_sendIOManagerEvent io_MANAGER_WAKEUP
                                  debugIO $ "woke up manager on thread: "
                                            ++ showThreadId t
                                WinIOScanning -> do
                                  debugIO $ "interrupted IOCP timeout wait on thread: "
                                            ++ showThreadId t
                                WinIOWaiting -> do
                                  debugIO $ "interrupted IOCP long wait on thread: "
                                            ++ showThreadId t
                                _             -> return ()
                               when (status /= WinIORunning)
                                    interruptSystemManager
                               return (Just t)

-- | The various states the I/O manager can be in. Used mostly for internal
-- bookkeeping and to make certain operations idempotent.
data WinIOStatus
  = WinIORunning  -- ^ I/O manager is running and doing something.
  | WinIOScanning -- ^ The I/O manager has been interrupted without servicing
                  -- a request. Likely due to a timer elapsing.
  | WinIOWaiting  -- ^ I/O manager is blocked on an alert-able wait for I/O
                  -- completions.
  | WinIOBlocked  -- ^ I/O manager is not servicing any I/O requests but the
                  -- thread is still alive.  This is usually the result of
                  -- a user requested event.
  | WinIODone     -- The I/O manager was requested to terminate and has done so.
  deriving Eq


statusWinIO :: MVar WinIOStatus
statusWinIO = unsafePerformIO $ newMVar WinIODone

setStatus :: WinIOStatus -> IO ()
setStatus val = modifyMVar_ statusWinIO (\_ -> return val)

getStatus :: IO WinIOStatus
getStatus = readMVar statusWinIO

requests :: MVar Word64
requests = unsafePerformIO $ newMVar 0

addRequest :: IO Word64
addRequest = modifyMVar requests (\x -> return (x + 1, x + 1))

removeRequest :: IO Word64
removeRequest = modifyMVar requests (\x -> return (x - 1, x - 1))

outstandingRequests :: IO Word64
outstandingRequests = withMVar requests return

getSystemManager :: IO Manager
getSystemManager = readMVar managerRef

-- | Mutable reference to the IO manager
managerRef :: MVar Manager
managerRef = unsafePerformIO $ newManager >>= newMVar
{-# NOINLINE managerRef #-}

-- | Interrupts an I/O manager Wait.  This will force the I/O manager to process
-- any outstanding events and timers.  Also called when console events such as
-- ctrl+c are used to break abort an I/O request.
interruptSystemManager :: IO ()
interruptSystemManager = do
  mgr <- getSystemManager
  status <- getStatus
  when (status /= WinIORunning) $
        do debugIO "interrupt received.."
           FFI.postQueuedCompletionStatus (mgrIOCP mgr) 0 0 nullPtr
  when (status == WinIODone) $
        do debugIO $  "I/O manager is dead. You need to revive it first. "
                   ++ "Try wakeupIOManager instead."


-- | The initial number of I/O requests we can service at the same time.
-- Must be power of 2.  This number is used as the starting point to scale
-- the number of concurrent requests.  It will be doubled everytime we are
-- saturated.
callbackArraySize :: Int
callbackArraySize = 32

-----------------------------------------------------------------------
-- Time utilities

secondsToNanoSeconds :: Seconds -> Q.Prio
secondsToNanoSeconds s = ceiling $ s * 1000000000

nanoSecondsToSeconds :: Q.Prio -> Seconds
nanoSecondsToSeconds n = fromIntegral n / 1000000000.0

------------------------------------------------------------------------
-- Overlapped I/O

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception, which 'withOverlapped'
-- will rethrow.
type StartCallback a = LPOVERLAPPED -> IO a

-- | Specialized callback type for I/O Completion Ports calls using
-- withOverlapped.
type StartIOCallback a = StartCallback (CbResult a)

-- | CallBack result type to disambiguate between the different states
-- an I/O Completion call could be in.
data CbResult a
  = CbDone (Maybe DWORD) -- ^ Request was handled immediately, no queue.
  | CbPending            -- ^ Queued and handled by I/O manager
  | CbIncomplete         -- ^ I/O request is incomplete but not enqueued, handle
                         --   it synchronously.
  | CbError a            -- ^ I/O request abort, return failure immediately
  | CbNone Bool          -- ^ The caller did not do any checking, the I/O
                         --   manager will perform additional checks.

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                          -> DWORD     -- ^ Number of bytes transferred
                          -> IO a

-- | Associate a 'HANDLE' with the current I/O manager's completion port.
-- This must be done before using the handle with 'withOverlapped'.
associateHandle' :: HANDLE -> IO ()
associateHandle' hwnd
  = do mngr <- getSystemManager
       associateHandle mngr hwnd

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    -- Use as completion key the file handle itself, so we can track completion
    FFI.associateHandleWithIOCP mgrIOCP h (fromIntegral $ ptrToWordPtr h)

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIoEx@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlappedEx :: Manager
                 -> String
                 -> HANDLE
                 -> Word64 -- ^ Value to use for the @OVERLAPPED@
                           --   structure's Offset/OffsetHigh members.
                 -> StartIOCallback Int
                 -> CompletionCallback (IOResult a)
                 -> IO (IOResult a)
withOverlappedEx mgr fname h offset startCB completionCB = do
    signal <- newEmptyIOPort :: IO (IOPort (IOResult a))
    let dbg s = s ++ " (" ++ show h ++ ":" ++ show offset ++ ")"
    let signalReturn a = failIfFalse_ (dbg "signalReturn") $
                            writeIOPort signal (IOSuccess a)
        signalThrow ex = failIfFalse_ (dbg "signalThrow") $
                            writeIOPort signal (IOFailed ex)
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        hs_lpol <- FFI.allocOverlapped offset
        -- Create the completion record and store it.
        -- We only need the record when we enqueue a request, however if we
        -- delay creating it then we will run into a race condition where the
        -- driver may have finished servicing the request before we were ready
        -- and so the request won't have the book keeping information to know
        -- what to do.  So because of that we always create the payload,  If we
        -- need it ok, if we don't that's no problem.  This approach prevents
        -- expensive lookups in hash-tables.
        --
        -- Todo: Use a memory pool for this so we don't have to hit malloc every
        --       time.  This would allow us to scale better.
        cdData <- new (CompletionData h completionCB')
        let ptr_lpol = hs_lpol `plusPtr` cdOffset
        poke ptr_lpol cdData
        let lpol = castPtr hs_lpol
        debugIO $ "hs_lpol:" ++ show hs_lpol
                ++ " cdData:" ++ show cdData
                ++ " ptr_lpol:" ++ show ptr_lpol

        execute <- startCB lpol `onException`
                        (CbError `fmap` Win32.getLastError) >>= \result -> do
          -- Check to see if the operation was completed on a
          -- non-overlapping handle or was completed immediately.
          -- e.g. stdio redirection or data in cache, FAST I/O.
          success <- FFI.overlappedIOStatus lpol
          err     <- fmap fromIntegral getLastError
          -- Determine if the caller has done any checking.  If not then check
          -- to see if the request was completed synchronously.  We have to
          -- in order to prevent deadlocks since if it has completed
          -- synchronously we've requested to not have the completion queued.
          let result' =
                case result of
                  CbNone ret | success == #{const STATUS_SUCCESS}      -> CbDone Nothing
                             | success == #{const STATUS_END_OF_FILE}  -> CbDone Nothing
                             | success == #{const STATUS_PENDING}      -> CbPending
                             -- Buffer was too small.. not sure what to do, so I'll just
                             -- complete the read request
                             | err     == #{const ERROR_MORE_DATA}     -> CbDone Nothing
                             | err     == #{const ERROR_SUCCESS}       -> CbDone Nothing
                             | err     == #{const ERROR_IO_PENDING}    -> CbPending
                             | err     == #{const ERROR_IO_INCOMPLETE} -> CbIncomplete
                             | err     == #{const ERROR_HANDLE_EOF}    -> CbDone Nothing
                             | not ret                                 -> CbError err
                             | otherwise                               -> CbPending
                  _                                                    -> result
          case result' of
            CbNone    _ -> error "shouldn't happen."
            CbIncomplete -> do
               debugIO $ "handling incomplete request synchronously " ++ show (h, lpol)
               res <- spinWaitComplete h lpol
               debugIO $ "done blocking request " ++ show (h, lpol)
               return res
            CbPending   -> do
              -- Before we enqueue check to see if operation finished in the
              -- mean time, since caller may not have done this.
              -- Normally we'd have to clear lpol with 0 before this call,
              -- however the statuses we're interested in would not get to here
              -- so we can save the memset call.
              finished <- FFI.getOverlappedResult h lpol False
              debugIO $ "== " ++ show (finished)
              status <- FFI.overlappedIOStatus lpol
              debugIO $ "== >< " ++ show (status)
              lasterr <- fmap fromIntegral getLastError :: IO Int
              -- This status indicated that we have finished early and so we
              -- won't have a request enqueued.  Handle it inline.
              let done_early = status == #{const STATUS_SUCCESS}
                               || status == #{const STATUS_END_OF_FILE}
                               || lasterr == #{const ERROR_HANDLE_EOF}
                               || lasterr == #{const ERROR_SUCCESS}
              -- This status indicates that the request hasn't finished early,
              -- but it will finish shortly.  The I/O manager will not be
              -- enqueuing this either.  Also needs to be handled inline.
              let will_finish_sync = lasterr == #{const ERROR_IO_INCOMPLETE}

              debugIO $ "== >*< " ++ show (finished, done_early, will_finish_sync, h, lpol, lasterr)
              case (finished, done_early, will_finish_sync) of
                (Nothing, False, False) -> do
                    reqs <- addRequest
                    debugIO $ "+1.. " ++ show reqs ++ " requests queued. | " ++ show lpol
                    wakeupIOManager
                    return result'
                (Nothing, False, True) -> do
                  debugIO $ "handling incomplete request synchronously " ++ show (h, lpol)
                  res <- spinWaitComplete h lpol
                  debugIO $ "done blocking request " ++ show (h, lpol)
                  return res
                _                -> do
                  debugIO "request handled immediately (o/b), not queued."
                  return $ CbDone finished
            CbError err' -> signalThrow (Just err') >> return result'
            CbDone  _   -> do
              debugIO "request handled immediately (o), not queued." >> return result'

        let cancel e = do
                        debugIO $ "## Exception occurred. Cancelling request... "
                        debugIO $ show (e :: SomeException)
                        _ <- uninterruptibleMask_ $ FFI.cancelIoEx' h lpol
                        -- we need to wait for the cancellation before removing
                        -- the pointer.
                        debugIO $ "## Waiting for cancellation record... "
                        _ <- FFI.getOverlappedResult h lpol True
                        let oldDataPtr = exchangePtr ptr_lpol nullReq
                        -- Check if we have to free and cleanup pointer
                        when (oldDataPtr == cdData) $
                          do free oldDataPtr
                             free hs_lpol
                             reqs <- removeRequest
                             debugIO $ "-1.. " ++ show reqs ++ " requests queued after error."
                             status <- fmap fromIntegral getLastError
                             completionCB' status 0
                        when (not threaded) $
                          do num_remaining <- outstandingRequests
                             servicedIOEntries num_remaining
                        return $ IOFailed Nothing
        let runner = do debugIO $ (dbg ":: waiting ") ++ " | "  ++ show lpol
                        res <- readIOPort signal `catch` cancel
                        debugIO $ dbg ":: signaled "
                        case res of
                          IOFailed err -> FFI.throwWinErr fname (maybe 0 fromIntegral err)
                          _            -> return res

        -- Sometimes we shouldn't bother with the I/O manager as the call has
        -- failed or is done.
        case execute of
          CbPending    -> runner
          CbDone rdata -> do
            -- free cdData
            debugIO $ dbg $ ":: done " ++ show lpol ++ " - " ++ show rdata
            bytes <- if isJust rdata
                        then return rdata
                        -- Make sure it's safe to free the OVERLAPPED buffer
                        else FFI.getOverlappedResult h lpol False
            case bytes of
              Just res ->  completionCB 0 res -- free hs_lpol >> completionCB 0 res
              Nothing  -> do err <- FFI.overlappedIOStatus lpol
                             numBytes <- FFI.overlappedIONumBytes lpol
                             -- TODO: Remap between STATUS_ and ERROR_ instead
                             -- of re-interpret here. But for now, don't care.
                             let err' = fromIntegral err
                             -- free hs_lpol
                             completionCB err' (fromIntegral numBytes)
          CbError err  -> do
            free cdData
            free hs_lpol
            let err' = fromIntegral err
            completionCB err' 0
          _            -> do
            free cdData
            free hs_lpol
            error "unexpected case in `execute'"
      where spinWaitComplete fhndl lpol = do
              -- Wait for the request to finish as it was running before and
              -- The I/O manager won't enqueue it due to our optimizations to
              -- prevent context switches in such cases.
              res <- FFI.getOverlappedResult fhndl lpol False
              status <- FFI.overlappedIOStatus lpol
              case res of
                -- Uses an inline definition of threadDelay to prevent an import
                -- cycle.
                Nothing | status == #{const STATUS_END_OF_FILE} -> do
                  when (not threaded) completeSynchronousRequest
                  return $ CbDone res
                        | otherwise ->
                  do m <- newEmptyIOPort
                     let secs = 100 / 1000000.0
                     reg <- registerTimeout mgr secs $
                              writeIOPort m () >> return ()
                     readIOPort m `onException` unregisterTimeout mgr reg
                     spinWaitComplete fhndl lpol
                _       -> do
                   when (not threaded) completeSynchronousRequest
                   return $ CbDone res

-- Safe version of function
withOverlapped :: String
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartIOCallback Int
               -> CompletionCallback (IOResult a)
               -> IO (IOResult a)
withOverlapped fname h offset startCB completionCB = do
  mngr <- getSystemManager
  withOverlappedEx mngr fname h offset startCB completionCB

------------------------------------------------------------------------
-- I/O Utilities

-- | Process an IOResult and throw an exception back to the user if the action
-- has failed, or return the result.
withException :: String -> IO (IOResult a) -> IO a
withException name fn
 = do res <- fn
      case res of
       IOSuccess a         -> return a
       IOFailed (Just err) -> FFI.throwWinErr name $ fromIntegral err
       IOFailed Nothing    -> FFI.throwWinErr name 0

-- | Signal that the I/O action was successful.
ioSuccess :: a -> IO (IOResult a)
ioSuccess = return . IOSuccess

-- | Signal that the I/O action has failed with the given reason.
ioFailed :: Integral a => a -> IO (IOResult a)
ioFailed = return . IOFailed . Just . fromIntegral

------------------------------------------------------------------------
-- Timeouts

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later un-register or update the timeout.
-- The timeout is automatically unregistered when it fires.
--
-- The 'TimeoutCallback' will not be called more than once.
registerTimeout :: Manager -> Seconds -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr@Manager{..} relTime cb = do
    key <- newUnique mgrUniqueSource
    if relTime <= 0 then cb
    else do
      now <- getTime mgrClock
      let !expTime = secondsToNanoSeconds $ now + relTime
      editTimeouts mgr (Q.unsafeInsertNew key expTime cb)
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = secondsToNanoSeconds $ now + relTime
    editTimeouts mgr (Q.adjust (const expTime) key)

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
    editTimeouts mgr (Q.delete key)

-- | Modify an existing timeout.  This isn't thread safe and so if the time to
-- elapse the timer was close it may fire anyway.
editTimeouts :: Manager -> TimeoutEdit -> IO ()
editTimeouts mgr g = do
  atomicModifyIORef' (mgrTimeouts mgr) $ \tq -> (g tq, ())
  wakeupIOManager

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next
-- | expiration.
runExpiredTimeouts :: Manager -> IO (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    now <- getTime mgrClock
    (expired, delay) <- atomicModifyIORef' mgrTimeouts (mkTimeout now)
    -- Execute timeout callbacks.
    mapM_ Q.value expired
    when (not threaded && not (null expired))
      completeSynchronousRequest
    debugIO $ "expired calls: " ++ show (length expired)
    return delay
      where
        mkTimeout :: Seconds -> TimeoutQueue ->
                     (TimeoutQueue, ([Q.Elem TimeoutCallback], Maybe Seconds))
        mkTimeout now tq =
            let (tq', (expired, sec)) = mkTimeout' (secondsToNanoSeconds now) tq
            in (tq', (expired, fmap nanoSecondsToSeconds sec))
        mkTimeout' :: Q.Prio -> TimeoutQueue ->
                     (TimeoutQueue, ([Q.Elem TimeoutCallback], Maybe Q.Prio))
        mkTimeout' now tq =
           -- Remove timeouts with expiration <= now.
           let (expired, tq') = Q.atMost now tq in
           -- See how soon the next timeout expires.
           case Q.prio `fmap` Q.findMin tq' of
            Nothing ->
                (tq', (expired, Nothing))
            Just t ->
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                in (tq', (expired, Just t'))

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

-- | Perform one full evaluation step of the I/O manager's service loop.
-- This means process timeouts and completed completions and calculate the time
-- for the next timeout.
--
-- The I/O manager is then notified of how long it should block again based on
-- the queued I/O requests and timers.  If the I/O manager was given a command
-- to block, shutdown or suspend than that request is honored at the end of the
-- loop.
step :: Bool -> Manager -> IO (Bool, Maybe Seconds)
step maxDelay mgr@Manager{..} = do
    -- Determine how long to wait the next time we block in an alertable state.
    delay <- runExpiredTimeouts mgr
    let !timer = if maxDelay && delay == Nothing
                    then #{const INFINITE}
                    else fromTimeout delay
    debugIO $ "next timeout: " ++ show delay
    debugIO $ "next timer: " ++ show timer -- todo: print as hex
    case (maxDelay, delay) of
      (_    , Just{} ) -> do setStatus WinIOWaiting
                             debugIO "I/O manager waiting."
      (False, Nothing) -> do setStatus WinIOScanning
                             debugIO "I/O manager pausing."
      (True , Nothing) -> do setStatus WinIOBlocked
                             debugIO "I/O manager deep sleep."
    -- If threaded this call informs the threadpool that a thread is now
    -- entering a kernel mode wait and this is free to be used.  If non-threaded
    -- then this is a no-op.
    notifyWaiting mgrThreadPool
    n <- if threaded
    -- To quote Matt Godbolts:
    -- There are some unusual edge cases you need to deal with. The
    -- GetQueuedCompletionStatus function blocks a thread until there's
    -- work for it to do. Based on the return value, the number of bytes
    -- and the overlapped structure, there’s a lot of possible "reasons"
    -- for the function to have returned. Deciphering all the possible
    -- cases:
    --
    -- ------------------------------------------------------------------------
    -- Ret value | OVERLAPPED | # of bytes | Description
    -- ------------------------------------------------------------------------
    -- zero      | NULL       | n/a        | Call to GetQueuedCompletionStatus
    --   failed, and no data was dequeued from the IO port. This usually
    --   indicates an error in the parameters to GetQueuedCompletionStatus.
    --
    -- zero      | non-NULL   | n/a        | Call to GetQueuedCompletionStatus
    --   failed, but data was read or written. The thread must deal with the
    --   data (possibly freeing any associated buffers), but there is an error
    --   condition on the underlying HANDLE. Usually seen when the other end of
    --   a network connection has been forcibly closed but there's still data in
    --   the send or receive queue.
    --
    -- non-zero  | NULL       | n/a        | This condition doesn't happen due
    --   to IO requests, but is useful to use in combination with
    --   PostQueuedCompletionStatus as a way of indicating to threads that they
    --   should terminate.
    --
    -- non-zero  | non-NULL   | zero       | End of file for a file HANDLE, or
    --   the connection has been gracefully closed (for network connections).
    --   The OVERLAPPED buffer has still been used; and must be deallocated if
    --   necessary.
    --
    -- non-zero  | non-NULL   | non-zero   | "num bytes" of data have been
    --    transferred into the block pointed by the OVERLAPPED structure. The
    --    direction of the transfer is dependant on the call made to the IO
    --    port, it's up to the user to remember if it was a read or a write
    --    (usually by stashing extra data in the OVERLAPPED structure). The
    --    thread must deallocate the structure as necessary.
    --
    -- The getQueuedCompletionStatusEx call will remove entries queued by the OS
    -- and returns the finished ones in mgrOverlappedEntries and the number of
    -- entries removed.
            then FFI.getQueuedCompletionStatusEx mgrIOCP mgrOverlappedEntries timer
            else do num_req <- outstandingRequests
                    registerAlertableWait mgrIOCP timer num_req
                    return 0
    setStatus WinIORunning
    -- If threaded this call informs the threadpool manager that a thread is
    -- busy.  If all threads are busy and we have not reached the maximum amount
    -- of allowed threads then the threadpool manager will spawn a new thread to
    -- allow us to scale under load.
    notifyRunning mgrThreadPool
    processCompletion mgr n delay

-- | Process the results at the end of an evaluation loop.  This function will
-- read all the completions, wake up all the Haskell threads, clean up the book
-- keeping of the I/O manager and return whether there are outstanding work to
-- be done and how long it expects to have to wait till it can take action
-- again.
--
-- Note that this method can do less work than there are entries in the
-- completion table.  This is because some completion entries may have been
-- created due to calls to interruptIOManager which will enqueue a faux
-- completion.
--
-- NOTE: In Threaded mode things get a bit complicated the operation may have
-- been completed even before we even got around to put the request in the
-- waiting callback table.  These events are handled by having a separate queue
-- for orphaned callback instances that the calling thread is supposed to check
-- before adding something to the work queue.
processCompletion :: Manager -> Int -> Maybe Seconds -> IO (Bool, Maybe Seconds)
processCompletion Manager{..} n delay = do
    -- If some completions are done, we need to process them and call their
    -- callbacks.  We then remove the callbacks from the bookkeeping and resize
    -- the index if required.
    when (n > 0) $ do
      forM_ [0..(n-1)] $ \idx -> do
        oe <- A.unsafeRead mgrOverlappedEntries idx
        let lpol     = lpOverlapped oe
        when (lpol /= nullPtr) $ do
          let hs_lpol  = castPtr lpol :: Ptr FFI.HASKELL_OVERLAPPED
          let ptr_lpol = castPtr (hs_lpol `plusPtr` cdOffset) :: Ptr (Ptr CompletionData)
          cdDataCheck <- peek ptr_lpol
          debugIO $ " $ checking " ++ show lpol
                    ++ " -en ptr_lpol: " ++ show ptr_lpol
                    ++ " offset: " ++ show cdOffset
                    ++ " cdData: " ++ show cdDataCheck
                    ++ " at idx " ++ show idx
          let oldDataPtr = exchangePtr ptr_lpol nullReq
          when (oldDataPtr /= nullReq) $
            do payload <- peek oldDataPtr
               debugIO $ "exchanged: " ++ show oldDataPtr
               let !(CompletionData _hwnd cb) = payload
               -- free oldDataPtr
               reqs <- removeRequest
               debugIO $ "-1.. " ++ show reqs ++ " requests queued."
               status <- FFI.overlappedIOStatus (lpOverlapped oe)
               -- TODO: Remap between STATUS_ and ERROR_ instead
               -- of re-interpret here. But for now, don't care.
               let status' = fromIntegral status
               cb status' (dwNumberOfBytesTransferred oe)
               -- free hs_lpol

      -- clear the array so we don't erroneously interpret the output, in
      -- certain circumstances like lockFileEx the code could return 1 entry
      -- removed but the file data not been filled in.
      -- TODO: Maybe not needed..
      A.clear mgrOverlappedEntries

      -- Check to see if we received the maximum amount of entries we could
      -- this likely indicates a high number of I/O requests have been queued.
      -- In which case we should process more at a time.
      cap <- A.capacity mgrOverlappedEntries
      when (cap == n) $ A.ensureCapacity mgrOverlappedEntries (2*cap)

    -- Keep running if we still have some work queued or
    -- if we have a pending delay.
    reqs <- outstandingRequests
    debugIO $ "outstanding requests: " ++ show reqs
    let more = reqs > 0
    debugIO $ "has more: " ++ show more ++ " - removed: " ++  show n
    return (more || (isJust delay && threaded), delay)

-- | Entry point for the non-threaded I/O manager to be able to process
-- completed completions.  It is mostly a wrapper around processCompletion.
processRemoteCompletion :: IO ()
processRemoteCompletion = do
  alloca $ \ptr_n -> do
    debugIO "processRemoteCompletion :: start ()"
    -- First figure out how much work we have to do.
    entries <- getOverlappedEntries ptr_n
    n <- fromIntegral `fmap` peek ptr_n
    -- This call will unmarshal data from the C buffer but pointers inside of
    -- this have not been read yet.
    _ <- peekArray n entries
    mngr <- getSystemManager
    let arr = mgrOverlappedEntries mngr
    A.unsafeSplat arr entries n
    _ <- processCompletion mngr n Nothing
    num_left <- outstandingRequests
    -- This call will unblock the non-threaded I/O manager.  After this it is no
    -- longer safe to use `entries` nor `completed`.
    servicedIOEntries num_left
    setStatus WinIOBlocked
    -- We may have been woken up due to a timer timeout.  So check for any
    -- expired timeouts. If we have processed any completions only check
    -- timeouts, if we have been woken up only to process timeouts then check if
    -- we have to change the wait interval.
    --
    -- When not the threaded runtime we would not have reset the timer events
    -- below.  Because of this when the request is done we have an additional
    -- step here to reset the wait timers so the I/O manager doesn't keep
    -- polling at the temporary high frequency we entered.
    if (n == 0)
       then step True mngr >> return ()
       else runExpiredTimeouts mngr >> return ()
    debugIO "processRemoteCompletion :: done ()"
    return ()

-- | Even loop for the Threaded I/O manager.  The one for the non-threaded
-- I/O manager is in AsyncWinIO.c in the rts.
io_mngr_loop :: HANDLE -> Manager -> IO ()
io_mngr_loop _event mgr = go False
    where
      go maxDelay =
          do setStatus WinIORunning
             (more, delay) <- step maxDelay mgr
             debugIO "I/O manager stepping."
             r2 <- c_readIOManagerEvent
             exit <-
               case r2 of
                 _ | r2 == io_MANAGER_WAKEUP -> return False
                 _ | r2 == io_MANAGER_DIE    -> return True
                 0 -> return False -- spurious wakeup
                 _ -> do debugIO $ "handling console event: " ++ show (r2 `shiftR` 1)
                         start_console_handler (r2 `shiftR` 1)
                         return False

             -- If we have no more work to do, or something from the outside
             -- told us to stop then we let the thread die and stop the I/O
             -- manager.  It will be woken up again when there is more to do.
             case () of
               _ | exit -> do setStatus WinIODone
                              debugIO "I/O manager shutting down."
               _ | not threaded -> do setStatus WinIOBlocked
                                      debugIO "I/O manager single threaded halt."
               _ | isJust delay -> go False
               -- We seem to have more work but no ETA for it.
               -- So just retry until we run out of work.
               _ | more -> go False
               _        -> go True

io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = #{const IO_MANAGER_WAKEUP}
io_MANAGER_DIE    = #{const IO_MANAGER_DIE}

-- | Wake up a single thread from the I/O Manager's worker queue.  This will
-- unblock a thread blocked in `processCompletion` and allows the I/O manager to
-- react accordingly to changes in timers or to process console signals.
wakeupIOManager :: IO ()
wakeupIOManager
  = do mngr <- getSystemManager
       status <- getStatus
       when (status /= WinIORunning) $ do
         event <- c_getIOManagerEvent
         debugIO "waking up I/O manager."
         startIOManagerThread (io_mngr_loop event mngr)

foreign import ccall unsafe "getIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_getIOManagerEvent :: IO HANDLE

foreign import ccall unsafe "readIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_readIOManagerEvent :: IO Word32

foreign import ccall unsafe "sendIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_sendIOManagerEvent :: Word32 -> IO ()

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool


-- ---------------------------------------------------------------------------
-- I/O manager event notifications


data HandleData = HandleData {
      tokenKey        :: {-# UNPACK #-} !HandleKey
    , tokenEvents     :: {-# UNPACK #-} !EventLifetime
    , _handleCallback :: !EventCallback
    }

-- | A file handle registration cookie.
data HandleKey = HandleKey {
      handleValue  :: {-# UNPACK #-} !HANDLE
    , handleUnique :: {-# UNPACK #-} !Unique
    } deriving ( Eq   -- ^ @since 4.4.0.0
               , Show -- ^ @since 4.4.0.0
               )

-- | Callback invoked on I/O events.
type EventCallback = HandleKey -> Event -> IO ()

registerHandle :: Manager -> EventCallback -> HANDLE -> Event -> Lifetime
               -> IO HandleKey
registerHandle (Manager{..}) cb hwnd evs lt = do
  u <- newUnique mgrUniqueSource
  let reg   = HandleKey hwnd u
      hwnd' = fromIntegral $ ptrToIntPtr hwnd
      el    = I.eventLifetime evs lt
      !hwdd = HandleData reg el cb
      event = EventData evs [(evs, hwdd)]
  _ <- withMVar mgrEvntHandlers $ \evts -> do
          IT.insertWith mappend hwnd' event evts
  wakeupIOManager
  return reg

unregisterHandle :: Manager -> HandleKey -> IO ()
unregisterHandle (Manager{..}) key@HandleKey{..} = do
  withMVar mgrEvntHandlers $ \evts -> do
    let hwnd' = fromIntegral $ ptrToIntPtr handleValue
    val <- IT.lookup hwnd' evts
    case val of
      Nothing -> return ()
      Just (EventData evs lst) -> do
        let cmp (_, a) (_, b) = tokenKey a == tokenKey b
            key'    = (undefined, HandleData key undefined undefined)
            updated = deleteBy cmp key' lst
            new_lst = EventData evs updated
        _ <- IT.updateWith (\_ -> return new_lst) hwnd' evts
        return ()


-- ---------------------------------------------------------------------------
-- debugging

#if defined(DEBUG)
c_DEBUG_DUMP :: IO Bool
c_DEBUG_DUMP = return True -- scheduler `fmap` getDebugFlags
#endif

debugIO :: String -> IO ()
#if defined(DEBUG)
debugIO s
  = do debug <- c_DEBUG_DUMP
       if debug
          then do tid <- myThreadId
                  let pref = if threaded then "\t" else ""
                  _   <- withCStringLen (pref ++ "winio: " ++ s ++ " (" ++
                                         showThreadId tid ++ ")\n") $
                         \(p, len) -> c_write 2 (castPtr p) (fromIntegral len)
                  return ()
          else do return ()
#else
debugIO _ = return ()
#endif

dbxIO :: String -> IO ()
dbxIO s = do tid <- myThreadId
             let pref = if threaded then "\t" else ""
             _   <- withCStringLen (pref ++ "winio: " ++ s ++ " (" ++
                                   showThreadId tid ++ ")\n") $
                   \(p, len) -> c_write 2 (castPtr p) (fromIntegral len)
             return ()