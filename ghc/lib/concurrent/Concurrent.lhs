%
% (c) The AQUA Project, Glasgow University, 1994-1996
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
	module ChannelVar,
	module Channel,
	module Semaphore,
	module Merge,
	module SampleVar,
	module PrelConc
    ) where

import Parallel
import ChannelVar
import Channel
import Semaphore
import Merge
import SampleVar
import PrelConc
\end{code}
