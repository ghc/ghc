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
	 putCVar,	--:: CVar a -> a -> IO ()
	 getCVar,	--:: CVar a -> IO a
	 _MVar

       ) where

import PreludeGlaST
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, takeMVar, _MVar
			)
\end{code}

@MVars@ provide the basic mechanisms for synchronising access to a shared
resource. @CVars@, or channel variables, provide an abstraction that guarantee
that the producer is not allowed to run riot, but enforces the interleaved
access to the channel variable,i.e., a producer is forced to wait up for
a consumer to remove the previous value before it can deposit a new one in the @CVar@.

\begin{code}

data CVar a
 = CVar (_MVar a)     -- prod -> cons
        (_MVar ())    -- cons -> prod

newCVar :: IO (CVar a)
putCVar :: CVar a -> a -> IO ()
getCVar :: CVar a -> IO a

newCVar 
 = newEmptyMVar >>= \ datum ->
   newMVar ()   >>= \ ack ->
   return (CVar datum ack)

putCVar (CVar datum ack) val
 = takeMVar ack      >> 
   putMVar datum val >>
   return ()

getCVar (CVar datum ack)
 = takeMVar datum >>= \ val ->
   putMVar ack () >> 
   return val

\end{code}
