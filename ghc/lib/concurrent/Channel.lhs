%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Channel]{Unbounded Channels}

Standard, unbounded channel abstraction.

\begin{code}
module Channel
       (
	 {- abstract type defined -}
        Chan,

	 {- creator -}
	newChan,	 -- :: IO (Chan a)

	 {- operators -}
	putChan,	 -- :: Chan a -> a -> IO ()
	getChan,	 -- :: Chan a -> IO a
	dupChan,	 -- :: Chan a -> IO (Chan a)
	unGetChan,	 -- :: Chan a -> a -> IO ()

	 {- stream interface -}
	getChanContents, -- :: Chan a -> IO [a]
	putList2Chan	 -- :: Chan a -> [a] -> IO ()

       ) where

import Prelude
import IOBase	( IO(..) )		-- Suspicious!
import ConcBase
import STBase
import UnsafeST ( unsafeInterleavePrimIO )
\end{code}

A channel is represented by two @MVar@s keeping track of the two ends
of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
are used to handle consumers trying to read from an empty channel.

\begin{code}

data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a (Stream a)


\end{code}

See the Concurrent Haskell paper for a diagram explaining the
how the different channel operations proceed.

@newChan@ sets up the read and write end of a channel by initialising
these two @MVar@s with an empty @MVar@.

\begin{code}

newChan :: IO (Chan a)
newChan
 = newEmptyMVar	     >>= \ hole ->
   newMVar hole      >>= \ read ->
   newMVar hole      >>= \ write ->
   return (Chan read write)

\end{code}

To put an element on a channel, a new hole at the write end is created.
What was previously the empty @MVar@ at the back of the channel is then
filled in with a new stream element holding the entered value and the
new hole.

\begin{code}

putChan :: Chan a -> a -> IO ()
putChan (Chan read write) val
 = newEmptyMVar		    >>= \ new_hole ->
   takeMVar write	    >>= \ old_hole ->
   putMVar write new_hole   >> 
   putMVar old_hole (ChItem val new_hole) >>
   return ()


getChan :: Chan a -> IO a
getChan (Chan read write)
 = takeMVar read	  >>= \ rend ->
   takeMVar rend          >>= \ (ChItem val new_rend) ->
   putMVar read new_rend  >>
   return val


dupChan :: Chan a -> IO (Chan a)
dupChan (Chan read write)
 = newEmptyMVar		  >>= \ new_read ->
   readMVar write	  >>= \ hole ->
   putMVar new_read hole  >>
   return (Chan new_read write)

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan read write) val
 = newEmptyMVar			      >>= \ new_rend ->
   takeMVar read		      >>= \ rend ->
   putMVar new_rend (ChItem val rend) >> 
   putMVar read new_rend              >>
   return ()

\end{code}

Operators for interfacing with functional streams.

\begin{code}

getChanContents :: Chan a -> IO [a]
getChanContents ch
{- WAS:
  = unsafeInterleavePrimIO (
      getChan ch 				   `thenPrimIO` \ ~(Right x) ->
      unsafeInterleavePrimIO (getChanContents ch)  `thenPrimIO` \ ~(Right xs) ->
      returnPrimIO  (Right (x:xs)))
-}
  = my_2_IO $ unsafeInterleavePrimIO (
	getChan_prim ch 			         >>= \ ~(Right x) ->
	unsafeInterleavePrimIO (getChanContents_prim ch) >>= \ ~(Right xs) ->
	returnPrimIO  (Right (x:xs)))

my_2_IO :: PrimIO (Either IOError a) -> IO a -- simple; primIOToIO does too much!
my_2_IO m = IO m

getChan_prim	     :: Chan a -> PrimIO (Either IOError  a)
getChanContents_prim :: Chan a -> PrimIO (Either IOError [a])

getChan_prim ch = ST $ \ s ->
    case (getChan ch) of { IO (ST get) ->
    get s }

getChanContents_prim ch = ST $ \ s ->
    case (getChanContents ch) of { IO (ST get) ->
    get s }

-------------
putList2Chan :: Chan a -> [a] -> IO ()
putList2Chan ch ls = sequence (map (putChan ch) ls)

\end{code}
