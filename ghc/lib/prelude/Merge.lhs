%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Merge]{Mergeing streams}

Avoiding the loss of ref. transparency by attaching the merge to the
IO monad.

\begin{code}
module Merge

	(
	 mergeIO,	--:: [a]   -> [a] -> IO [a]
	 nmergeIO	--:: [[a]] -> IO [a]
	) where

import Semaphore

import PreludeGlaST
import Concurrent	( forkIO )
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, takeMVar, _MVar
			)
\end{code}

\begin{code}

max_buff_size = 1

mergeIO :: [a] -> [a] -> IO [a]
nmergeIO :: [[a]] -> IO [a]

#ifndef __CONCURRENT_HASKELL__

mergeIO _ _  = return []
nmergeIO _   = return []

#else

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
 = (_MVar (_MVar [a]), QSem)

suckIO :: _MVar Int -> Buffer a -> [a] -> IO ()

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
		unsafeInterleavePrimIO (
			takeMVar next_node       `thenPrimIO` \ (Right x) ->
			signalQSem e             `seqPrimIO`
			returnPrimIO x)          `thenPrimIO` \ next_node_val ->
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
    mapIO f xs = accumulate (map f xs)
\end{code}

So as to avoid creating a mutual recursive module dep. with @Concurrent.lhs@,
the defn. of @forkIO@ is duplicated here:

\begin{code}
{- HAH! WDP 95/07

forkIO :: PrimIO a -> PrimIO a
forkIO action s
 = let
    (r, new_s) = action s
   in
    new_s `_fork_` (r, s)
 where
    _fork_ x y = case (fork# x) of { 0# -> parError#; _ -> y }
-}

#endif {- __CONCURRENT_HASKELL__ -}

\end{code}
