{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
           , RankNTypes
  #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- kludge for the Control.Concurrent.QSem, Control.Concurrent.QSemN
-- and Control.Concurrent.SampleVar imports.

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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
        myThreadId,

        forkIO,
        forkFinally,
        forkIOWithUnmask,
        killThread,
        throwTo,

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

        -- ** Waiting
        threadDelay,
        threadWaitRead,
        threadWaitWrite,
        threadWaitReadSTM,
        threadWaitWriteSTM,

        -- * Communication abstractions

        module GHC.Internal.Control.Concurrent.MVar,
        module Control.Concurrent.Chan,
        module Control.Concurrent.QSem,
        module Control.Concurrent.QSemN,

        -- * Bound Threads
        -- $boundthreads
        rtsSupportsBoundThreads,
        forkOS,
        forkOSWithUnmask,
        isCurrentThreadBound,
        runInBoundThread,
        runInUnboundThread,

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

        -- ** Deadlock

        -- $deadlock

    ) where

import Prelude
import GHC.Internal.Control.Exception.Base as Exception

import GHC.Internal.Conc.Bound
import GHC.Conc hiding (threadWaitRead, threadWaitWrite,
                        threadWaitReadSTM, threadWaitWriteSTM)

import GHC.Internal.System.Posix.Types ( Fd )

#if defined(mingw32_HOST_OS)
import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.C.Types
import GHC.Internal.System.IO
import GHC.Internal.Data.Functor ( void )
import GHC.Internal.Int ( Int64 )
#else
import qualified GHC.Internal.Conc.IO as Conc
#endif

import GHC.Internal.Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN

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
provided by the "Control.Concurrent" module.
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

-}

-- | Fork a thread and call the supplied function when the thread is about
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
-- @since 4.6.0.0
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

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
#if defined(mingw32_HOST_OS)
  -- we have no IO manager implementing threadWaitRead on Windows.
  -- fdReady does the right thing, but we have to call it in a
  -- separate thread, otherwise threadWaitRead won't be interruptible,
  -- and this only works with -threaded.
  | threaded  = withThread "threadWaitRead worker" (waitFd fd False)
  | otherwise = case fd of
                  0 -> do _ <- hWaitForInput stdin (-1)
                          return ()
                        -- hWaitForInput does work properly, but we can only
                        -- do this for stdin since we know its FD.
                  _ -> errorWithoutStackTrace "threadWaitRead requires -threaded on Windows, or use GHC.System.IO.hWaitForInput"
#else
  = Conc.threadWaitRead fd
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
#if defined(mingw32_HOST_OS)
  | threaded  = withThread "threadWaitWrite worker" (waitFd fd True)
  | otherwise = errorWithoutStackTrace "threadWaitWrite requires -threaded on Windows"
#else
  = Conc.threadWaitWrite fd
#endif

-- | Returns an STM action that can be used to wait for data
-- to read from a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
--
-- @since 4.7.0.0
threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM fd
#if defined(mingw32_HOST_OS)
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do
                    tid <- myThreadId
                    labelThread tid "threadWaitReadSTM worker"
                    result <- try (waitFd fd False)
                    atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = errorWithoutStackTrace "threadWaitReadSTM requires -threaded on Windows"
#else
  = Conc.threadWaitReadSTM fd
#endif

-- | Returns an STM action that can be used to wait until data
-- can be written to a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
--
-- @since 4.7.0.0
threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM fd
#if defined(mingw32_HOST_OS)
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do
                    tid <- myThreadId
                    labelThread tid "threadWaitWriteSTM worker"
                    result <- try (waitFd fd True)
                    atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = errorWithoutStackTrace "threadWaitWriteSTM requires -threaded on Windows"
#else
  = Conc.threadWaitWriteSTM fd
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

withThread :: String -> IO a -> IO a
withThread label io = do
  m <- newEmptyMVar
  _ <- mask_ $ forkIO $ do
    tid <- myThreadId
    labelThread tid label
    result <- try io
    putMVar m result
  x <- takeMVar m
  case x of
    Right a -> return a
    Left e  -> throwIO (e :: IOException)

waitFd :: Fd -> Bool -> IO ()
waitFd fd write = do
   throwErrnoIfMinus1_ "fdReady" $
        fdReady (fromIntegral fd) (if write then 1 else 0) (-1) 0

foreign import ccall safe "fdReady"
  fdReady :: CInt -> CBool -> Int64 -> CBool -> IO CInt
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

      The "System.IO" module manages multiplexing in its own way.  On
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
      "System.IO" module still does multiplexing, so there can be multiple
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

{- $deadlock

GHC attempts to detect when threads are deadlocked using the garbage
collector.  A thread that is not reachable (cannot be found by
following pointers from live objects) must be deadlocked, and in this
case the thread is sent an exception.  The exception is either
'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM',
'NonTermination', or 'Deadlock', depending on the way in which the
thread is deadlocked.

Note that this feature is intended for debugging, and should not be
relied on for the correct operation of your program.  There is no
guarantee that the garbage collector will be accurate enough to detect
your deadlock, and no guarantee that the garbage collector will run in
a timely enough manner.  Basically, the same caveats as for finalizers
apply to deadlock detection.

There is a subtle interaction between deadlock detection and
finalizers (as created by 'GHC.Foreign.Concurrent.newForeignPtr' or the
functions in "System.Mem.Weak"): if a thread is blocked waiting for a
finalizer to run, then the thread will be considered deadlocked and
sent an exception.  So preferably don't do this, but if you have no
alternative then it is possible to prevent the thread from being
considered deadlocked by making a 'StablePtr' pointing to it.  Don't
forget to release the 'StablePtr' later with 'freeStablePtr'.
-}
