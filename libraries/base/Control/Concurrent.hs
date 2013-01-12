{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- kludge for the Control.Concurrent.QSem, Control.Concurrent.QSemN
-- and Control.Concurrent.SampleVar imports.

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------

module Control.Concurrent (
        -- * Concurrent Haskell

        -- $conc_intro

        -- * Basic concurrency operations

        ThreadId,
#ifdef __GLASGOW_HASKELL__
        myThreadId,
#endif

        forkIO,
#ifdef __GLASGOW_HASKELL__
        forkFinally,
        forkIOWithUnmask,
        killThread,
        throwTo,
#endif

        -- ** Threads with affinity
        forkOn,
        forkOnWithUnmask,
        getNumCapabilities,
        setNumCapabilities,
        threadCapability,

        -- * Scheduling

        -- $conc_scheduling     
        yield,

        -- ** Blocking

        -- $blocking

#ifdef __GLASGOW_HASKELL__
        -- ** Waiting
        threadDelay,
        threadWaitRead,
        threadWaitWrite,
        threadWaitReadSTM,
        threadWaitWriteSTM,
#endif

        -- * Communication abstractions

        module Control.Concurrent.MVar,
        module Control.Concurrent.Chan,
        module Control.Concurrent.QSem,
        module Control.Concurrent.QSemN,

#ifdef __GLASGOW_HASKELL__
        -- * Bound Threads
        -- $boundthreads
        rtsSupportsBoundThreads,
        forkOS,
        isCurrentThreadBound,
        runInBoundThread,
        runInUnboundThread,
#endif

        -- * Weak references to ThreadIds
        mkWeakThreadId,

        -- * GHC's implementation of concurrency

        -- |This section describes features specific to GHC's
        -- implementation of Concurrent Haskell.

        -- ** Haskell threads and Operating System threads

        -- $osthreads

        -- ** Terminating the program

        -- $termination

        -- ** Pre-emption

        -- $preemption

        -- * Deprecated functions
        forkIOUnmasked

    ) where

import Prelude

import Control.Exception.Base as Exception

#ifdef __GLASGOW_HASKELL__
import GHC.Exception
import GHC.Conc hiding (threadWaitRead, threadWaitWrite,
                        threadWaitReadSTM, threadWaitWriteSTM)
import qualified GHC.Conc
import GHC.IO           ( IO(..), unsafeInterleaveIO, unsafeUnmask )
import GHC.IORef        ( newIORef, readIORef, writeIORef )
import GHC.Base

import System.Posix.Types ( Fd )
import Foreign.StablePtr
import Foreign.C.Types
import Control.Monad    ( when )

#ifdef mingw32_HOST_OS
import Foreign.C
import System.IO
import Data.Maybe (Maybe(..))
#endif
#endif

#ifdef __HUGS__
import Hugs.ConcBase
#endif

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN

#ifdef __HUGS__
type ThreadId = ()
#endif

{- $conc_intro

The concurrency extension for Haskell is described in the paper
/Concurrent Haskell/
<http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz>.

Concurrency is \"lightweight\", which means that both thread creation
and context switching overheads are extremely low.  Scheduling of
Haskell threads is done internally in the Haskell runtime system, and
doesn't make use of any operating system-supplied thread packages.

However, if you want to interact with a foreign library that expects your
program to use the operating system-supplied thread package, you can do so
by using 'forkOS' instead of 'forkIO'.

Haskell threads can communicate via 'MVar's, a kind of synchronised
mutable variable (see "Control.Concurrent.MVar").  Several common
concurrency abstractions can be built from 'MVar's, and these are
provided by the "Control.Concurrent" library.
In GHC, threads may also communicate via exceptions.
-}

{- $conc_scheduling

    Scheduling may be either pre-emptive or co-operative,
    depending on the implementation of Concurrent Haskell (see below
    for information related to specific compilers).  In a co-operative
    system, context switches only occur when you use one of the
    primitives defined in this module.  This means that programs such
    as:


>   main = forkIO (write 'a') >> write 'b'
>     where write c = putChar c >> write c

    will print either @aaaaaaaaaaaaaa...@ or @bbbbbbbbbbbb...@,
    instead of some random interleaving of @a@s and @b@s.  In
    practice, cooperative multitasking is sufficient for writing
    simple graphical user interfaces.  
-}

{- $blocking
Different Haskell implementations have different characteristics with
regard to which operations block /all/ threads.

Using GHC without the @-threaded@ option, all foreign calls will block
all other Haskell threads in the system, although I\/O operations will
not.  With the @-threaded@ option, only foreign calls with the @unsafe@
attribute will block all other threads.

Using Hugs, all I\/O operations and foreign calls will block all other
Haskell threads.
-}

-- | fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
--
-- > forkFinally action and_then =
-- >   mask $ \restore ->
-- >     forkIO $ try (restore action) >>= and_then
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
--
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

#ifdef __GLASGOW_HASKELL__
-- ---------------------------------------------------------------------------
-- Bound Threads

{- $boundthreads
   #boundthreads#

Support for multiple operating system threads and bound threads as described
below is currently only available in the GHC runtime system if you use the
/-threaded/ option when linking.

Other Haskell systems do not currently support multiple operating system threads.

A bound thread is a haskell thread that is /bound/ to an operating system
thread. While the bound thread is still scheduled by the Haskell run-time
system, the operating system thread takes care of all the foreign calls made
by the bound thread.

To a foreign library, the bound thread will look exactly like an ordinary
operating system thread created using OS functions like @pthread_create@
or @CreateThread@.

Bound threads can be created using the 'forkOS' function below. All foreign
exported functions are run in a bound thread (bound to the OS thread that
called the function). Also, the @main@ action of every Haskell program is
run in a bound thread.

Why do we need this? Because if a foreign library is called from a thread
created using 'forkIO', it won't have access to any /thread-local state/ - 
state variables that have specific values for each OS thread
(see POSIX's @pthread_key_create@ or Win32's @TlsAlloc@). Therefore, some
libraries (OpenGL, for example) will not work from a thread created using
'forkIO'. They work fine in threads created using 'forkOS' or when called
from @main@ or from a @foreign export@.

In terms of performance, 'forkOS' (aka bound) threads are much more
expensive than 'forkIO' (aka unbound) threads, because a 'forkOS'
thread is tied to a particular OS thread, whereas a 'forkIO' thread
can be run by any OS thread.  Context-switching between a 'forkOS'
thread and a 'forkIO' thread is many times more expensive than between
two 'forkIO' threads.

Note in particular that the main program thread (the thread running
@Main.main@) is always a bound thread, so for good concurrency
performance you should ensure that the main thread is not doing
repeated communication with other threads in the system.  Typically
this means forking subthreads to do the work using 'forkIO', and
waiting for the results in the main thread.

-}

-- | 'True' if bound threads are supported.
-- If @rtsSupportsBoundThreads@ is 'False', 'isCurrentThreadBound'
-- will always return 'False' and both 'forkOS' and 'runInBoundThread' will
-- fail.
foreign import ccall rtsSupportsBoundThreads :: Bool


{- | 
Like 'forkIO', this sparks off a new thread to run the 'IO'
computation passed as the first argument, and returns the 'ThreadId'
of the newly created thread.

However, 'forkOS' creates a /bound/ thread, which is necessary if you
need to call foreign (non-Haskell) libraries that make use of
thread-local state, such as OpenGL (see "Control.Concurrent#boundthreads").

Using 'forkOS' instead of 'forkIO' makes no difference at all to the
scheduling behaviour of the Haskell runtime system.  It is a common
misconception that you need to use 'forkOS' instead of 'forkIO' to
avoid blocking all the Haskell threads when making a foreign call;
this isn't the case.  To allow foreign calls to be made without
blocking all the Haskell threads (with GHC), it is only necessary to
use the @-threaded@ option when linking your program, and to make sure
the foreign import is not marked @unsafe@.
-}

forkOS :: IO () -> IO ThreadId

foreign export ccall forkOS_entry
    :: StablePtr (IO ()) -> IO ()

foreign import ccall "forkOS_entry" forkOS_entry_reimported
    :: StablePtr (IO ()) -> IO ()

forkOS_entry :: StablePtr (IO ()) -> IO ()
forkOS_entry stableAction = do
        action <- deRefStablePtr stableAction
        action

foreign import ccall forkOS_createThread
    :: StablePtr (IO ()) -> IO CInt

failNonThreaded :: IO a
failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"

forkOS action0
    | rtsSupportsBoundThreads = do
        mv <- newEmptyMVar
        b <- Exception.getMaskingState
        let
            -- async exceptions are masked in the child if they are masked
            -- in the parent, as for forkIO (see #1048). forkOS_createThread
            -- creates a thread with exceptions masked by default.
            action1 = case b of
                        Unmasked -> unsafeUnmask action0
                        MaskedInterruptible -> action0
                        MaskedUninterruptible -> uninterruptibleMask_ action0

            action_plus = Exception.catch action1 childHandler

        entry <- newStablePtr (myThreadId >>= putMVar mv >> action_plus)
        err <- forkOS_createThread entry
        when (err /= 0) $ fail "Cannot create OS thread."
        tid <- takeMVar mv
        freeStablePtr entry
        return tid
    | otherwise = failNonThreaded

-- | Returns 'True' if the calling thread is /bound/, that is, if it is
-- safe to use foreign libraries that rely on thread-local state from the
-- calling thread.
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = IO $ \ s# ->
    case isCurrentThreadBound# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)


{- | 
Run the 'IO' computation passed as the first argument. If the calling thread
is not /bound/, a bound thread is created temporarily. @runInBoundThread@
doesn't finish until the 'IO' computation finishes.

You can wrap a series of foreign function calls that rely on thread-local state
with @runInBoundThread@ so that you can use them without knowing whether the
current thread is /bound/.
-}
runInBoundThread :: IO a -> IO a

runInBoundThread action
    | rtsSupportsBoundThreads = do
        bound <- isCurrentThreadBound
        if bound
            then action
            else do
                ref <- newIORef undefined
                let action_plus = Exception.try action >>= writeIORef ref
                bracket (newStablePtr action_plus)
                        freeStablePtr
                        (\cEntry -> forkOS_entry_reimported cEntry >> readIORef ref) >>=
                  unsafeResult
    | otherwise = failNonThreaded

{- | 
Run the 'IO' computation passed as the first argument. If the calling thread
is /bound/, an unbound thread is created temporarily using 'forkIO'.
@runInBoundThread@ doesn't finish until the 'IO' computation finishes.

Use this function /only/ in the rare case that you have actually observed a
performance loss due to the use of bound threads. A program that
doesn't need it's main thread to be bound and makes /heavy/ use of concurrency
(e.g. a web server), might want to wrap it's @main@ action in
@runInUnboundThread@.

Note that exceptions which are thrown to the current thread are thrown in turn
to the thread that is executing the given computation. This ensures there's
always a way of killing the forked thread.
-}
runInUnboundThread :: IO a -> IO a

runInUnboundThread action = do
  bound <- isCurrentThreadBound
  if bound
    then do
      mv <- newEmptyMVar
      mask $ \restore -> do
        tid <- forkIO $ Exception.try (restore action) >>= putMVar mv
        let wait = takeMVar mv `Exception.catch` \(e :: SomeException) ->
                     Exception.throwTo tid e >> wait
        wait >>= unsafeResult
    else action

unsafeResult :: Either SomeException a -> IO a
unsafeResult = either Exception.throwIO return
#endif /* __GLASGOW_HASKELL__ */

#ifdef __GLASGOW_HASKELL__
-- ---------------------------------------------------------------------------
-- threadWaitRead/threadWaitWrite

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use
-- 'GHC.Conc.closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#ifdef mingw32_HOST_OS
  -- we have no IO manager implementing threadWaitRead on Windows.
  -- fdReady does the right thing, but we have to call it in a
  -- separate thread, otherwise threadWaitRead won't be interruptible,
  -- and this only works with -threaded.
  | threaded  = withThread (waitFd fd 0)
  | otherwise = case fd of
                  0 -> do _ <- hWaitForInput stdin (-1)
                          return ()
                        -- hWaitForInput does work properly, but we can only
                        -- do this for stdin since we know its FD.
                  _ -> error "threadWaitRead requires -threaded on Windows, or use System.IO.hWaitForInput"
#else
  = GHC.Conc.threadWaitRead fd
#endif

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use
-- 'GHC.Conc.closeFdWith'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#ifdef mingw32_HOST_OS
  | threaded  = withThread (waitFd fd 1)
  | otherwise = error "threadWaitWrite requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitWrite fd
#endif

-- | Returns an STM action that can be used to wait for data
-- to read from a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM fd
#ifdef mingw32_HOST_OS
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ forkIO $ do result <- try (waitFd fd 0)
                                      atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = error "threadWaitReadSTM requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitReadSTM fd
#endif

-- | Returns an STM action that can be used to wait until data
-- can be written to a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM fd 
#ifdef mingw32_HOST_OS
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ forkIO $ do result <- try (waitFd fd 1)
                                      atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = error "threadWaitWriteSTM requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitWriteSTM fd
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

withThread :: IO a -> IO a
withThread io = do
  m <- newEmptyMVar
  _ <- mask_ $ forkIO $ try io >>= putMVar m
  x <- takeMVar m
  case x of
    Right a -> return a
    Left e  -> throwIO (e :: IOException)

waitFd :: Fd -> CInt -> IO ()
waitFd fd write = do
   throwErrnoIfMinus1_ "fdReady" $
        fdReady (fromIntegral fd) write iNFINITE 0

iNFINITE :: CInt
iNFINITE = 0xFFFFFFFF -- urgh

foreign import ccall safe "fdReady"
  fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
#endif

-- ---------------------------------------------------------------------------
-- More docs

{- $osthreads

      #osthreads# In GHC, threads created by 'forkIO' are lightweight threads, and
      are managed entirely by the GHC runtime.  Typically Haskell
      threads are an order of magnitude or two more efficient (in
      terms of both time and space) than operating system threads.

      The downside of having lightweight threads is that only one can
      run at a time, so if one thread blocks in a foreign call, for
      example, the other threads cannot continue.  The GHC runtime
      works around this by making use of full OS threads where
      necessary.  When the program is built with the @-threaded@
      option (to link against the multithreaded version of the
      runtime), a thread making a @safe@ foreign call will not block
      the other threads in the system; another OS thread will take
      over running Haskell threads until the original call returns.
      The runtime maintains a pool of these /worker/ threads so that
      multiple Haskell threads can be involved in external calls
      simultaneously.

      The "System.IO" library manages multiplexing in its own way.  On
      Windows systems it uses @safe@ foreign calls to ensure that
      threads doing I\/O operations don't block the whole runtime,
      whereas on Unix systems all the currently blocked I\/O requests
      are managed by a single thread (the /IO manager thread/) using
      a mechanism such as @epoll@ or @kqueue@, depending on what is
      provided by the host operating system.

      The runtime will run a Haskell thread using any of the available
      worker OS threads.  If you need control over which particular OS
      thread is used to run a given Haskell thread, perhaps because
      you need to call a foreign library that uses OS-thread-local
      state, then you need bound threads (see "Control.Concurrent#boundthreads").

      If you don't use the @-threaded@ option, then the runtime does
      not make use of multiple OS threads.  Foreign calls will block
      all other running Haskell threads until the call returns.  The
      "System.IO" library still does multiplexing, so there can be multiple
      threads doing I\/O, and this is handled internally by the runtime using
      @select@.
-}

{- $termination

      In a standalone GHC program, only the main thread is
      required to terminate in order for the process to terminate.
      Thus all other forked threads will simply terminate at the same
      time as the main thread (the terminology for this kind of
      behaviour is \"daemonic threads\").

      If you want the program to wait for child threads to
      finish before exiting, you need to program this yourself.  A
      simple mechanism is to have each child thread write to an
      'MVar' when it completes, and have the main
      thread wait on all the 'MVar's before
      exiting:

>   myForkIO :: IO () -> IO (MVar ())
>   myForkIO io = do
>     mvar <- newEmptyMVar
>     forkFinally io (\_ -> putMVar mvar ())
>     return mvar

      Note that we use 'forkFinally' to make sure that the
      'MVar' is written to even if the thread dies or
      is killed for some reason.

      A better method is to keep a global list of all child
      threads which we should wait for at the end of the program:

>    children :: MVar [MVar ()]
>    children = unsafePerformIO (newMVar [])
>    
>    waitForChildren :: IO ()
>    waitForChildren = do
>      cs <- takeMVar children
>      case cs of
>        []   -> return ()
>        m:ms -> do
>           putMVar children ms
>           takeMVar m
>           waitForChildren
>
>    forkChild :: IO () -> IO ThreadId
>    forkChild io = do
>        mvar <- newEmptyMVar
>        childs <- takeMVar children
>        putMVar children (mvar:childs)
>        forkFinally io (\_ -> putMVar mvar ())
>
>     main =
>       later waitForChildren $
>       ...

      The main thread principle also applies to calls to Haskell from
      outside, using @foreign export@.  When the @foreign export@ed
      function is invoked, it starts a new main thread, and it returns
      when this main thread terminates.  If the call causes new
      threads to be forked, they may remain in the system after the
      @foreign export@ed function has returned.
-}

{- $preemption

      GHC implements pre-emptive multitasking: the execution of
      threads are interleaved in a random fashion.  More specifically,
      a thread may be pre-empted whenever it allocates some memory,
      which unfortunately means that tight loops which do no
      allocation tend to lock out other threads (this only seems to
      happen with pathological benchmark-style code, however).

      The rescheduling timer runs on a 20ms granularity by
      default, but this may be altered using the
      @-i\<n\>@ RTS option.  After a rescheduling
      \"tick\" the running thread is pre-empted as soon as
      possible.

      One final note: the
      @aaaa@ @bbbb@ example may not
      work too well on GHC (see Scheduling, above), due
      to the locking on a 'System.IO.Handle'.  Only one thread
      may hold the lock on a 'System.IO.Handle' at any one
      time, so if a reschedule happens while a thread is holding the
      lock, the other thread won't be able to run.  The upshot is that
      the switch from @aaaa@ to
      @bbbbb@ happens infrequently.  It can be
      improved by lowering the reschedule tick period.  We also have a
      patch that causes a reschedule whenever a thread waiting on a
      lock is woken up, but haven't found it to be useful for anything
      other than this example :-)
-}
#endif /* __GLASGOW_HASKELL__ */
