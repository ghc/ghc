{-
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
-}

module Concurrent (
	forkIO,
	par, seq, fork, -- re-exported from GHCbase

	-- waiting on file descriptor I/O
	threadWaitRead, threadWaitWrite, 

	-- wait for timeout
        threadDelay,

	module ChannelVar,
	module Channel,
	module Semaphore,
	module Merge,
	module SampleVar,

	-- IVars and MVars come from here, too
	IVar, MVar,
	newEmptyMVar, takeMVar, putMVar, newMVar, readMVar, swapMVar,
	newIVar, readIVar, writeIVar

    ) where

import Parallel
import ChannelVar
import Channel
import Semaphore
import Merge
import SampleVar

import GHCbase

forkIO :: IO () -> IO ()

forkIO (IO (ST action)) = IO $ ST $ \ s ->
    let
	(_, new_s) = action s
    in
    new_s `fork` (Right (), s)
