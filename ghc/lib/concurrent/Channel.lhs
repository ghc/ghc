%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-97
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
	writeChan,	 -- :: Chan a -> a -> IO ()
	readChan,	 -- :: Chan a -> IO a
	dupChan,	 -- :: Chan a -> IO (Chan a)
	unGetChan,	 -- :: Chan a -> a -> IO ()

	 {- stream interface -}
	getChanContents, -- :: Chan a -> IO [a]
	writeList2Chan	 -- :: Chan a -> [a] -> IO ()

       ) where

import Prelude
import PrelConc
import PrelST
import PrelUnsafe ( unsafeInterleaveIO )
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
newChan = do
   hole  <- newEmptyMVar
   read  <- newMVar hole
   write <- newMVar hole
   return (Chan read write)
\end{code}

To put an element on a channel, a new hole at the write end is created.
What was previously the empty @MVar@ at the back of the channel is then
filled in with a new stream element holding the entered value and the
new hole.

\begin{code}
writeChan :: Chan a -> a -> IO ()
writeChan (Chan read write) val = do
   new_hole <- newEmptyMVar
   old_hole <- takeMVar write
   putMVar write new_hole
   putMVar old_hole (ChItem val new_hole)

readChan :: Chan a -> IO a
readChan (Chan read write) = do
  read_end		    <- takeMVar read
  (ChItem val new_read_end) <- takeMVar read_end
  putMVar read new_read_end
  return val


dupChan :: Chan a -> IO (Chan a)
dupChan (Chan read write) = do
   new_read <- newEmptyMVar
   hole     <- readMVar write
   putMVar new_read hole
   return (Chan new_read write)

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan read write) val = do
   new_read_end <- newEmptyMVar
   read_end     <- takeMVar read
   putMVar new_read_end (ChItem val read_end)
   putMVar read new_read_end

\end{code}

Operators for interfacing with functional streams.

\begin{code}
getChanContents :: Chan a -> IO [a]
getChanContents ch
  = unsafeInterleaveIO (do
	x  <- readChan ch
    	xs <- getChanContents ch
    	return (x:xs)
    )

-------------
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence (map (writeChan ch) ls)

\end{code}
