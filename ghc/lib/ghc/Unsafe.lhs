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
	  trace,
	  runST
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

Definition of runST
~~~~~~~~~~~~~~~~~~~

SLPJ 95/04: Why @runST@ must not have an unfolding; consider:
\begin{verbatim}
f x =
  runST ( \ s -> let
		    (a, s')  = newArray# 100 [] s
		    (_, s'') = fill_in_array_or_something a x s'
		  in
		  freezeArray# a s'' )
\end{verbatim}
If we inline @runST@, we'll get:
\begin{verbatim}
f x = let
	(a, s')  = newArray# 100 [] realWorld#{-NB-}
	(_, s'') = fill_in_array_or_something a x s'
      in
      freezeArray# a s''
\end{verbatim}
And now the @newArray#@ binding can be floated to become a CAF, which
is totally and utterly wrong:
\begin{verbatim}
f = let
    (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
    in
    \ x ->
	let (_, s'') = fill_in_array_or_something a x s' in
	freezeArray# a s''
\end{verbatim}
All calls to @f@ will share a {\em single} array!  End SLPJ 95/04.

\begin{code}
runST :: (All s => ST s a) -> a
runST st = 
  case st of
	ST m -> case m realWorld# of
      			STret _ r -> r
\end{code}
