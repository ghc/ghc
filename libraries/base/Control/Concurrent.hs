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

#ifndef __HUGS__
        ThreadId,
	myThreadId,
#endif

	forkIO,
#ifndef __HUGS__
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
import GHC.Conc
import GHC.TopHandler   ( reportStackOverflow, reportError )
import GHC.IOBase	( IO(..) )
import GHC.IOBase	( unsafeInterleaveIO )
import GHC.Base
#endif

#ifdef __HUGS__
import Hugs.ConcBase
#endif

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Concurrent.SampleVar

{- $conc_intro

The concurrency extension for Haskell is described in the paper
/Concurrent Haskell/
<http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz>.

Concurrency is \"lightweight\", which means that both thread creation
and context switching overheads are extremely low.  Scheduling of
Haskell threads is done internally in the Haskell runtime system, and
doesn't make use of any operating system-supplied thread packages.

Haskell threads can communicate via 'MVar's, a kind of synchronised
mutable variable (see "Control.Concurrent.MVar").  Several common
concurrency abstractions can be built from 'MVar's, and these are
provided by the "Concurrent" library.  Threads may also communicate
via exceptions. 
-}

{- $conc_scheduling

    Scheduling may be either pre-emptive or co-operative,
    depending on the implementation of Concurrent Haskell (see below
    for imformation related to specific compilers).  In a co-operative
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

foreign import ccall unsafe "cmp_thread" cmp_thread :: ThreadId# -> ThreadId# -> Int
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
	AsyncException ThreadKilled  -> return ()

	-- report all others:
	AsyncException StackOverflow -> reportStackOverflow False
	ErrorCall s -> reportError False s
	other       -> reportError False (showsPrec 0 other "\n")

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
>     mvar \<- newEmptyMVar
>     forkIO (io \`finally\` putMVar mvar ())
>     return mvar

      Note that we use 'finally' from the
      "Exception" module to make sure that the
      'MVar' is written to even if the thread dies or
      is killed for some reason.

      A better method is to keep a global list of all child
      threads which we should wait for at the end of the program:

>     children :: MVar [MVar ()]
>     children = unsafePerformIO (newMVar [])
>     
>     waitForChildren :: IO ()
>     waitForChildren = do
>     	(mvar:mvars) \<- takeMVar children
>     	putMVar children mvars
>     	takeMVar mvar
>     	waitForChildren
>     
>     forkChild :: IO () -> IO ()
>     forkChild io = do
>     	 mvar \<- newEmptyMVar
>     	 forkIO (p \`finally\` putMVar mvar ())
>     	 childs \<- takeMVar children
>     	 putMVar children (mvar:childs)
>     
>     later = flip finally
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
      happen with pathalogical benchmark-style code, however).

      The rescheduling timer runs on a 20ms granularity by
      default, but this may be altered using the
      @-i<n>@ RTS option.  After a rescheduling
      \"tick\" the running thread is pre-empted as soon as
      possible.

      One final note: the
      @aaaa@ @bbbb@ example may not
      work too well on GHC (see Scheduling, above), due
      to the locking on a 'Handle'.  Only one thread
      may hold the lock on a 'Handle' at any one
      time, so if a reschedule happens while a thread is holding the
      lock, the other thread won't be able to run.  The upshot is that
      the switch from @aaaa@ to
      @bbbbb@ happens infrequently.  It can be
      improved by lowering the reschedule tick period.  We also have a
      patch that causes a reschedule whenever a thread waiting on a
      lock is woken up, but haven't found it to be useful for anything
      other than this example :-)
-}
