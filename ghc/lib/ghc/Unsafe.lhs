%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Unsafe]{Module @Unsafe@}

These functions have their own module because we definitely don't want
them to be inlined.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Unsafe
        ( unsafePerformIO, 
	  unsafeInterleaveIO, 
   	  unsafeInterleaveST,
	  trace
        ) where
\end{code}

\begin{code}
import PrelBase
import IOBase
import STBase
import Addr
import {-# SOURCE #-} Error ( error )
\end{code}

%*********************************************************
%*							*
\subsection{Unsafe @IO@ operations}
%*							*
%*********************************************************

\begin{code}
unsafePerformIO	:: IO a -> a
unsafePerformIO (IO m)
  = case m realWorld# of
      IOok _ r   -> r
      IOfail _ e -> error ("unsafePerformIO: I/O error: " ++ show e ++ "\n")

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO m) = IO ( \ s ->
	let
	    IOok _ r = m s
	in
	IOok s r)

{-# GENERATE_SPECS _trace a #-}
trace :: String -> a -> a
trace string expr
  = unsafePerformIO (
	((_ccall_ PreTraceHook sTDERR{-msg-}):: IO ())  >>
	fputs sTDERR string				>>
	((_ccall_ PostTraceHook sTDERR{-msg-}):: IO ()) >>
	return expr )
  where
    sTDERR = (``stderr'' :: Addr)
\end{code}

\begin{code}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST ( \ s ->
    let
	STret _ r = m s
    in
    STret s r)

\end{code}
