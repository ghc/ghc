%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelUnsafe]{Module @PrelUnsafe@}

These functions have their own module because we definitely don't want
them to be inlined. The reason is that we may end up turning an action
into a constant when it is not:

  new :: IORef Int
  new = 
   let
    foo = unsafePerformIO getNextValue
   in
   newIORef foo 

If unsafePerformIO is inlined here, the application of getNextValue to the realWorld# 
token might be floated out, leaving us with

  foo' = getNextValue realWorld#

  new :: IORef Int
  new = newIORef foo'

which is not what we want.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelUnsafe
        ( unsafePerformIO, 
	  unsafeInterleaveIO, 
	  trace,
        ) where
\end{code}

\begin{code}
import PrelBase
import PrelIOBase
import PrelAddr
import {-# SOURCE #-} PrelErr ( error )
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
         res =
	   case m s of
	     IOok _ r   -> r
	     IOfail _ e -> error ("unsafeInterleaveIO: I/O error: " ++ show e ++ "\n")
	in
	IOok s res
    )


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
