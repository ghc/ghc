%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelST]{The @ST@ monad}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelST where

import Monad
import PrelBase
import PrelGHC
\end{code}

%*********************************************************
%*							*
\subsection{The @ST@ monad}
%*							*
%*********************************************************

The state-transformer monad proper.  By default the monad is strict;
too many people got bitten by space leaks when it was lazy.

\begin{code}
newtype ST s a = ST (State# s -> STret s a)

data STret s a = STret (State# s) a

instance Functor (ST s) where
    map f (ST m) = ST $ \ s ->
      case (m s) of { STret new_s r ->
      STret new_s (f r) }

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST $ \ s -> STret s x
    m >> k   =  m >>= \ _ -> k

    (ST m) >>= k
      = ST $ \ s ->
	case (m s) of { STret new_s r ->
	case (k r) of { ST k2 ->
	(k2 new_s) }}



fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let (ST k_r)  = k r
	ans       = k_r s
	STret _ r = ans
    in
    ans

{-# NOINLINE unsafeInterleaveST #-}
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
{-# NOINLINE runST #-}
runST :: (All s => ST s a) -> a
runST st = 
  case st of
	ST m -> case m realWorld# of
      			STret _ r -> r
\end{code}

%*********************************************************
%*							*
\subsection{Ghastly return types}
%*							*
%*********************************************************

The @State@ type is the return type of a _ccall_ with no result.  It
never actually exists, since it's always deconstructed straight away;
the desugarer ensures this.

\begin{code}
data State	     s     = S#		     (State# s)
data StateAndPtr#    s elt = StateAndPtr#    (State# s) elt 

data StateAndChar#   s     = StateAndChar#   (State# s) Char# 
data StateAndInt#    s     = StateAndInt#    (State# s) Int# 
data StateAndWord#   s     = StateAndWord#   (State# s) Word#
data StateAndFloat#  s     = StateAndFloat#  (State# s) Float# 
data StateAndDouble# s     = StateAndDouble# (State# s) Double#  
data StateAndAddr#   s     = StateAndAddr#   (State# s) Addr#
\end{code}
