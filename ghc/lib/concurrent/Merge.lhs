%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Merge]{Mergeing streams}

Avoiding the loss of ref. transparency by attaching the merge to the
IO monad.

(The ops. are now defined in Concurrent to avoid module loop trouble).

\begin{code}
module Merge
	(
	  merge
	, nmergeIO
	) where

import Concurrent
\end{code}
