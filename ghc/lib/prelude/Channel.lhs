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

import PreludeGlaST
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, takeMVar, _MVar
			)
\end{code}

A channel is represented by two @MVar@s keeping track of the two ends
of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
are used to handle consumers trying to read from an empty channel.

\begin{code}

data Chan a
 = Chan (_MVar (Stream a))
        (_MVar (Stream a))

type Stream a = _MVar (ChItem a)

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

\end{code}

\begin{code}

getChan :: Chan a -> IO a
getChan (Chan read write)
 = takeMVar read	  >>= \ rend ->
   takeMVar rend          >>= \ (ChItem val new_rend) ->
   putMVar read new_rend  >>
   return val

\end{code}

\begin{code}

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
getChanContents ch =
 unsafeInterleavePrimIO (
  getChan ch 				       `thenPrimIO` \ ~(Right x) ->
  unsafeInterleavePrimIO (getChanContents ch)  `thenPrimIO` \ ~(Right xs) ->
  returnPrimIO  (Right (x:xs)))

putList2Chan :: Chan a -> [a] -> IO ()
putList2Chan ch ls = sequence (map (putChan ch) ls)

\end{code}
