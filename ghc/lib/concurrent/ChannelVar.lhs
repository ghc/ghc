%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[ChannelVar]{Channel variables}

Channel variables, are one-element channels described in the Concurrent
Haskell paper (available from @ftp://ftp.dcs.gla.ac.uk/pub/glasgow-fp/drafts@)

\begin{code}
module ChannelVar
       (
	 {- abstract -}
         CVar,
	 newCVar,	--:: IO (CVar a)
	 writeCVar,	--:: CVar a -> a -> IO ()
	 readCVar,	--:: CVar a -> IO a
	 MVar

       ) where

import Prelude
import ConcBase
\end{code}

@MVars@ provide the basic mechanisms for synchronising access to a shared
resource. @CVars@, or channel variables, provide an abstraction that guarantee
that the producer is not allowed to run riot, but enforces the interleaved
access to the channel variable,i.e., a producer is forced to wait up for
a consumer to remove the previous value before it can deposit a new one in the @CVar@.

\begin{code}

data CVar a
 = CVar (MVar a)     -- prod -> cons
        (MVar ())    -- cons -> prod

newCVar :: IO (CVar a)
writeCVar :: CVar a -> a -> IO ()
readCVar :: CVar a -> IO a

newCVar 
 = newEmptyMVar >>= \ datum ->
   newMVar ()   >>= \ ack ->
   return (CVar datum ack)

writeCVar (CVar datum ack) val
 = takeMVar ack      >> 
   putMVar datum val >>
   return ()

readCVar (CVar datum ack)
 = takeMVar datum >>= \ val ->
   putMVar ack () >> 
   return val
\end{code}
