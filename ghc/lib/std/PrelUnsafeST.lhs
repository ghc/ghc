%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[UnsafeST]{Module @UnsafeST@}

These functions have their own module because we definitely don't want
them to be inlined.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelUnsafeST (unsafeInterleaveST, runST) where

import PrelST
import PrelBase
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

