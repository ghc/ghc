%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Semaphore]{Quantity semaphores}

General/quantity semaphores

\begin{code}
module Semaphore

      (
       {- abstract -}
       QSem,

       newQSem,		--:: Int  -> IO QSem
       waitQSem,	--:: QSem -> IO ()
       signalQSem,	--:: QSem -> IO ()

       {- abstract -}
       QSemN,
       newQSemN,	--:: Int   -> IO QSemN
       waitQSemN,	--:: QSemN -> Int -> IO ()
       signalQSemN	--:: QSemN -> Int -> IO ()
	
      ) where

import PreludeGlaST
import PreludePrimIO	( newEmptyMVar, newMVar, putMVar,
			  readMVar, takeMVar, _MVar
			)
\end{code}

General semaphores are also implemented readily in terms of shared @MVar@s,
only have to catch the case when the semaphore is tried waited on
when it is empty (==0). Implement this in the same way as shared variables are
implemented - maintaining a list of @MVar@s representing threads currently
waiting. The counter is a shared variable, ensuring the mutual exclusion on its access.

\begin{code}

data QSem = QSem (_MVar (Int, [_MVar ()]))

newQSem :: Int -> IO QSem
newQSem init 
 = newMVar (init,[])	  >>= \ sem ->
   return (QSem sem)

waitQSem :: QSem -> IO ()
waitQSem (QSem sem)
 = takeMVar sem 	>>= \ (avail,blocked) ->    -- gain ex. access
   if avail > 0 then
     putMVar sem (avail-1,[]) >> 
     return ()
   else
     newEmptyMVar       >>= \ block ->
     {-
	Stuff the reader at the back of the queue,
	so as to preserve waiting order. A signalling
	process then only have to pick the MVar at the
	front of the blocked list.

	The version of waitQSem given in the paper could
	lead to starvation.
     -}
     putMVar sem (0, blocked++[block]) >> 
     takeMVar block		       >>= \ v ->
     return v

signalQSem :: QSem -> IO ()
signalQSem (QSem sem)
 = takeMVar sem   >>= \ (avail,blocked) ->
   case blocked of
     [] -> putMVar sem (avail+1,[]) >>
	   return ()
     (block:blocked') ->
	   putMVar sem (0,blocked') >>
	   putMVar block ()         >>
	   return ()

\end{code}

\begin{code}

data QSemN
 = QSemN (_MVar (Int,[(Int,_MVar ())]))

newQSemN :: Int -> IO QSemN 
newQSemN init 
 = newMVar (init,[])	  >>= \ sem ->
   return (QSemN sem)

waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN sem) sz
 = takeMVar sem >>= \ (avail,blocked) ->    -- gain ex. access
   if avail > 0 then
     putMVar sem (avail-1,[]) >>
     return ()
   else
     newEmptyMVar 		            >>= \ block ->
     putMVar sem (0, blocked++[(sz,block)]) >> 
     takeMVar block			    >>
     return ()


signalQSemN :: QSemN -> Int  -> IO ()
signalQSemN (QSemN sem) n
 = takeMVar sem   	  	 >>= \ (avail,blocked) ->
   free (avail+n) blocked 	 >>= \ (avail',blocked') ->
   putMVar sem (avail',blocked') >>
   return ()
   where
    free avail [] = return (avail,[])
    free avail ((req,block):blocked) =
     if avail > req then
	putMVar block () >>
	free (avail-req) blocked
     else
	free avail blocked >>= \ (avail',blocked') ->
        return (avail',(req,block):blocked')


\end{code}
