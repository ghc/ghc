%
% (c) The AQUA Project, Glasgow University, 1995
%
\section[Concurrent]{Concurrent Haskell constructs}

A common interface to a collection of useful concurrency abstractions.
Currently, the collection only contains the abstractions found in the
{\em Concurrent Haskell} paper (presented at the Haskell Workshop
1995, draft available via \tr{ftp} from
\tr{ftp.dcs.gla.ac.uk/pub/glasgow-fp/drafts}.)  plus a couple of
others. See the paper and the individual files containing the module
definitions for explanation on what they do.

\begin{code}
module Concurrent (
	forkIO,
	par, seq, -- reexported from Parallel

	threadWait, threadDelay,

	ChannelVar..,
	Channel..,
	Semaphore..,
	Merge..,
	SampleVar..,

	-- IVars and MVars come from here, too
	IVar(..), MVar(..), -- for convenience...
	_IVar, _MVar,	-- abstract
	newEmptyMVar, takeMVar, putMVar, newMVar, readMVar, swapMVar,
	newIVar, readIVar, writeIVar

    ) where

import Parallel
import ChannelVar
import Channel
import Semaphore
import Merge
import SampleVar

import PreludeGlaST	( forkST )
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, swapMVar, takeMVar, _MVar,
			  newIVar, readIVar, writeIVar, _IVar,
			  IVar(..), MVar(..),
			  threadWait, threadDelay
			)

forkIO :: IO () -> IO ()

forkIO action s
  = let
	(_, new_s) = action s
    in
    new_s `_fork_` (Right (), s)
 where
    _fork_ x y = case (fork# x) of { 0# -> parError#; _ -> y }
\end{code}
