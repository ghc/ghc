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
	killThread,
	throwTo,
#endif

	-- * Scheduling

	-- $conc_scheduling	
	yield,         		-- :: IO ()

	-- ** Blocking
	
	-- $blocking

#ifdef __GLASGOW_HASKELL__
	-- ** Waiting
	threadDelay,		-- :: Int -> IO ()
	threadWaitRead,		-- :: Int -> IO ()
	threadWaitWrite,	-- :: Int -> IO ()
#endif

	-- * Communication abstractions

	module Control.Concurrent.MVar,
	module Control.Concurrent.Chan,
	module Control.Concurrent.QSem,
	module Control.Concurrent.QSemN,
	module Control.Concurrent.SampleVar,

	-- * Merging of streams
#ifndef __HUGS__
	mergeIO,		-- :: [a]   -> [a] -> IO [a]
	nmergeIO,		-- :: [[a]] -> IO [a]
#endif
	-- $merge

#ifdef __GLASGOW_HASKELL__
	-- * Bound Threads
	-- $boundthreads
	rtsSupportsBoundThreads,
	forkOS,
	isCurrentThreadBound,
	runInBoundThread,
	runInUnboundThread
#endif

	-- * GHC's implementation of concurrency

	-- |This section describes features specific to GHC's
	-- implementation of Concurrent Haskell.
	
	-- ** Terminating the program

	-- $termination

	-- ** Pre-emption

	-- $preemption
    ) where

import Prelude

import Control.Exception as Exception

#ifdef __GLASGOW_HASKELL__
import GHC.Conc		( ThreadId(..), myThreadId, killThread, yield,
			  threadDelay, threadWaitRead, threadWaitWrite )
import GHC.TopHandler   ( reportStackOverflow, reportError )
import GHC.IOBase	( IO(..) )
import GHC.IOBase	( unsafeInterleaveIO )
import GHC.IOBase	( newIORef, readIORef, writeIORef )
import GHC.Base

import Foreign.StablePtr
import Foreign.C.Types  ( CInt )
import Control.Monad    ( when )
#endif

#ifdef __HUGS__
import Hugs.ConcBase
#endif

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Concurrent.SampleVar

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
Calling a foreign C procedure (such as @getchar@) that blocks waiting
for input will block /all/ threads, unless the @threadsafe@ attribute
is used on the foreign call (and your compiler \/ operating system
supports it).  GHC's I\/O system uses non-blocking I\/O internally to
implement thread-friendly I\/O, so calling standard Haskell I\/O
functions blocks only the thread making the call.
-}

-- Thread Ids, specifically the instances of Eq and Ord for these things.
-- The ThreadId type itself is defined in std/PrelConc.lhs.

-- Rather than define a new primitve, we use a little helper function
-- cmp_thread in the RTS.

#ifdef __GLASGOW_HASKELL__
id2TSO :: ThreadId -> ThreadId#
id2TSO (ThreadId t) = t

foreign import ccall unsafe "cmp_thread" cmp_thread :: ThreadId# -> ThreadId# -> CInt
-- Returns -1, 0, 1

cmpThread :: ThreadId -> ThreadId -> Ordering
cmpThread t1 t2 = 
   case cmp_thread (id2TSO t1) (id2TSO t2) of
      -1 -> LT
      0  -> EQ
      _  -> GT -- must be 1

instance Eq ThreadId where
   t1 == t2 = 
      case t1 `cmpThread` t2 of
         EQ -> True
         _  -> False

instance Ord ThreadId where
   compare = cmpThread

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> Int

instance Show ThreadId where
   showsPrec d t = 
   	showString "ThreadId " . 
        showsPrec d (getThreadId (id2TSO t))

{- |
This sparks off a new thread to run the 'IO' computation passed as the
first argument, and returns the 'ThreadId' of the newly created
thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.
-}
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = Exception.catch action childHandler

childHandler :: Exception -> IO ()
childHandler err = Exception.catch (real_handler err) childHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	-- ignore thread GC and killThread exceptions:
	BlockedOnDeadMVar            -> return ()
	BlockedIndefinitely          -> return ()
	AsyncException ThreadKilled  -> return ()

	-- report all others:
	AsyncException StackOverflow -> reportStackOverflow
	other       -> reportError other

#endif /* __GLASGOW_HASKELL__ */

#ifndef __HUGS__
max_buff_size :: Int
max_buff_size = 1

mergeIO :: [a] -> [a] -> IO [a]
nmergeIO :: [[a]] -> IO [a]

-- $merge
-- The 'mergeIO' and 'nmergeIO' functions fork one thread for each
-- input list that concurrently evaluates that list; the results are
-- merged into a single output list.  
--
-- Note: Hugs does not provide these functions, since they require
-- preemptive multitasking.

mergeIO ls rs
 = newEmptyMVar		       >>= \ tail_node ->
   newMVar tail_node	       >>= \ tail_list ->
   newQSem max_buff_size       >>= \ e ->
   newMVar 2                   >>= \ branches_running ->
   let
    buff = (tail_list,e)
   in
    forkIO (suckIO branches_running buff ls) >>
    forkIO (suckIO branches_running buff rs) >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val

type Buffer a 
 = (MVar (MVar [a]), QSem)

suckIO :: MVar Int -> Buffer a -> [a] -> IO ()

suckIO branches_running buff@(tail_list,e) vs
 = case vs of
	[] -> takeMVar branches_running >>= \ val ->
	      if val == 1 then
		 takeMVar tail_list     >>= \ node ->
		 putMVar node []        >>
		 putMVar tail_list node
	      else 	
  		 putMVar branches_running (val-1)
	(x:xs) ->
		waitQSem e 	   		 >>
		takeMVar tail_list 		 >>= \ node ->
	        newEmptyMVar 	   		 >>= \ next_node ->
		unsafeInterleaveIO (
			takeMVar next_node  >>= \ y ->
			signalQSem e	    >>
			return y)	         >>= \ next_node_val ->
		putMVar node (x:next_node_val)   >>
		putMVar tail_list next_node 	 >>
		suckIO branches_running buff xs

nmergeIO lss
 = let
    len = length lss
   in
    newEmptyMVar	  >>= \ tail_node ->
    newMVar tail_node	  >>= \ tail_list ->
    newQSem max_buff_size >>= \ e ->
    newMVar len		  >>= \ branches_running ->
    let
     buff = (tail_list,e)
    in
    mapIO (\ x -> forkIO (suckIO branches_running buff x)) lss >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val
  where
    mapIO f xs = sequence (map f xs)
#endif /* __HUGS__ */

#ifdef __GLASGOW_HASKELL__
-- ---------------------------------------------------------------------------
-- Bound Threads

{- $boundthreads

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
-}

-- | 'True' if bound threads are supported.
-- If @rtsSupportsBoundThreads@ is 'False', 'isCurrentThreadBound'
-- will always return 'False' and both 'forkOS' and 'runInBoundThread' will
-- fail.
foreign import ccall rtsSupportsBoundThreads :: Bool


{- |
Like 'forkIO', this sparks off a new thread to run the 'IO' computation passed as the
first argument, and returns the 'ThreadId' of the newly created
thread.

However, @forkOS@ uses operating system-supplied multithreading support to create
a new operating system thread. The new thread is /bound/, which means that
all foreign calls made by the 'IO' computation are guaranteed to be executed
in this new operating system thread; also, the operating system thread is not
used for any other foreign calls.

This means that you can use all kinds of foreign libraries from this thread 
(even those that rely on thread-local state), without the limitations of 'forkIO'.
-}
forkOS :: IO () -> IO ThreadId

foreign export ccall forkOS_entry
    :: StablePtr (IO ()) -> IO ()

foreign import ccall "forkOS_entry" forkOS_entry_reimported
    :: StablePtr (IO ()) -> IO ()

forkOS_entry stableAction = do
	action <- deRefStablePtr stableAction
	action

foreign import ccall forkOS_createThread
    :: StablePtr (IO ()) -> IO CInt

failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"
    
forkOS action 
    | rtsSupportsBoundThreads = do
	mv <- newEmptyMVar
	let action_plus = Exception.catch action childHandler
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
		resultOrException <- 
		    bracket (newStablePtr action_plus)
			    freeStablePtr
			    (\cEntry -> forkOS_entry_reimported cEntry >> readIORef ref)
		case resultOrException of
		    Left exception -> Exception.throw exception
		    Right result -> return result
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
-}
runInUnboundThread :: IO a -> IO a

runInUnboundThread action = do
    bound <- isCurrentThreadBound
    if bound
        then do
            mv <- newEmptyMVar
            forkIO (Exception.try action >>= putMVar mv)
            takeMVar mv >>= \either -> case either of
                Left exception -> Exception.throw exception
                Right result -> return result
        else action
	
#endif /* __GLASGOW_HASKELL__ */

-- ---------------------------------------------------------------------------
-- More docs

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
>     forkIO (io `finally` putMVar mvar ())
>     return mvar

      Note that we use 'finally' from the
      "Control.Exception" module to make sure that the
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
>    	    putMVar children ms
>    	    takeMVar m
>    	    waitForChildren
>    
>    forkChild :: IO () -> IO ()
>    forkChild io = do
>    	 mvar <- newEmptyMVar
>    	 childs <- takeMVar children
>    	 putMVar children (mvar:childs)
>    	 forkIO (io `finally` putMVar mvar ())
>
>     main =
>     	later waitForChildren $
>     	...

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
