%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Error]{Module @Error@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Error (errorIO, error) where

import PrelBase
import IOBase
import Foreign
import Addr
\end{code}

%*********************************************************
%*							*
\subsection{Error-ish functions}
%*							*
%*********************************************************

\begin{code}
errorIO :: IO () -> a

errorIO (IO io)
  = case (errorIO# io) of
      _ -> bottom
  where
    bottom = bottom -- Never evaluated

--errorIO x = (waitRead#, errorIO#, makeForeignObj#, waitWrite#, (+#))

-- error stops execution and displays an error message
error :: String -> a
error s = error__ ( \ x -> _ccall_ ErrorHdrHook x ) s

error__ :: (Addr{-FILE *-} -> IO ()) -> String -> a

error__ msg_hdr s
#ifdef __PARALLEL_HASKELL__
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ stg_exit (1::Int)
	    )
#else
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ getErrorHandler	>>= \ errorHandler ->
	     if errorHandler == (-1::Int) then
		_ccall_ stg_exit (1::Int)
	     else
		_casm_ ``%r = (StgStablePtr)(%0);'' errorHandler
						>>= \ osptr ->
		_ccall_ decrementErrorCount     >>= \ () ->
		deRefStablePtr osptr            >>= \ oact ->
		oact
	    )
#endif {- !parallel -}
  where
    sTDERR = (``stderr'' :: Addr)
\end{code}

