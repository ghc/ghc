%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[SampleVar]{Sample variables}

Sample variables are slightly different from a normal @MVar@:

\begin{itemize}
\item Reading an empty @SampleVar@ causes the reader to block.
    (same as @takeMVar@ on empty @MVar@)
\item Reading a filled @SampleVar@ empties it and returns value.
    (same as @takeMVar@)
\item Writing to an empty @SampleVar@ fills it with a value, and
potentially, wakes up a blocked reader  (same as for @putMVar@ on empty @MVar@).
\item Writing to a filled @SampleVar@ overwrites the current value.
 (different from @putMVar@ on full @MVar@.)
\end{itemize}

\begin{code}
module SampleVar
       (
         SampleVar(..),    --:: type _ =
 
         newSampleVar,     --:: IO (SampleVar a)
	 emptySampleVar,   --:: SampleVar a -> IO ()
	 readSample,  	   --:: SampleVar a -> IO a
	 writeSample  	   --:: SampleVar a -> a -> IO ()

       ) where

import PreludeGlaST
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, swapMVar, takeMVar, _MVar
			)
\end{code}

\begin{code}

type SampleVar a
 = _MVar (Int,		-- 1  == full
			-- 0  == empty
			-- <0 no of readers blocked
          _MVar a)

\end{code}

Initally, a @SampleVar@ is empty/unfilled.

\begin{code}

newSampleVar :: IO (SampleVar a)
newSampleVar
 = newEmptyMVar          >>= \ val ->
   newMVar (0,val)

emptySampleVar :: SampleVar a -> IO ()
emptySampleVar v
 = takeMVar v         >>= \ (readers,var) ->
   if readers >= 0 then
     putMVar v (0,var)
   else
     putMVar v (readers,var)

\end{code}



\begin{code}

--
-- filled => make empty and grab sample
-- not filled => try to grab value, empty when read val.
--
readSample :: SampleVar a -> IO a
readSample svar
 = takeMVar svar                >>= \ (readers,val) ->
   putMVar svar (readers-1,val) >>
   takeMVar val

--
-- filled => overwrite
-- not filled => fill, write val
--
writeSample :: SampleVar a -> a -> IO ()
writeSample svar v
 = takeMVar svar  >>= \ (readers, val) ->
   case readers of
     1 -> 
       swapMVar val v 	    >> 
       putMVar svar (1,val)
     _ -> 
       putMVar val v >> 
       putMVar svar (min 1 (readers+1), val)

\end{code}
