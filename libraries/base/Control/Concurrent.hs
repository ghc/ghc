-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Concurrent.hs,v 1.3 2001/12/21 15:07:21 simonmar Exp $
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------

module Control.Concurrent (
	module Control.Concurrent.Chan,
	module Control.Concurrent.CVar,
	module Control.Concurrent.MVar,
	module Control.Concurrent.QSem,
	module Control.Concurrent.QSemN,
	module Control.Concurrent.SampleVar,

	forkIO,			-- :: IO () -> IO ()
	yield,         		-- :: IO ()

#ifdef __GLASGOW_HASKELL__
        ThreadId,

	-- Forking and suchlike
	myThreadId, 		-- :: IO ThreadId
	killThread,		-- :: ThreadId -> IO ()
	throwTo,		-- :: ThreadId -> Exception -> IO ()

	threadDelay,		-- :: Int -> IO ()
	threadWaitRead,		-- :: Int -> IO ()
	threadWaitWrite,	-- :: Int -> IO ()
#endif

	 -- merging of streams
	mergeIO,		-- :: [a]   -> [a] -> IO [a]
	nmergeIO		-- :: [[a]] -> IO [a]
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
import IOExts ( unsafeInterleaveIO )
import ConcBase
#endif

import Control.Concurrent.MVar
import Control.Concurrent.CVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Concurrent.SampleVar

-- Thread Ids, specifically the instances of Eq and Ord for these things.
-- The ThreadId type itself is defined in std/PrelConc.lhs.

-- Rather than define a new primitve, we use a little helper function
-- cmp_thread in the RTS.

#ifdef __GLASGOW_HASKELL__
foreign import ccall "cmp_thread" unsafe cmp_thread :: Addr# -> Addr# -> Int
-- Returns -1, 0, 1

cmpThread :: ThreadId -> ThreadId -> Ordering
cmpThread (ThreadId t1) (ThreadId t2) = 
   case cmp_thread (unsafeCoerce# t1) (unsafeCoerce# t2) of
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


max_buff_size :: Int
max_buff_size = 1

mergeIO :: [a] -> [a] -> IO [a]
nmergeIO :: [[a]] -> IO [a]

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
