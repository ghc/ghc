\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ST
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'ST' Monad.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.ST where

import GHC.Base
import GHC.Show
import GHC.Num

default ()
\end{code}

%*********************************************************
%*							*
\subsection{The @ST@ monad}
%*							*
%*********************************************************

The state-transformer monad proper.  By default the monad is strict;
too many people got bitten by space leaks when it was lazy.

\begin{code}
-- | The strict state-transformer monad.
-- A computation of type @'ST' s a@ transforms an internal state indexed
-- by @s@, and returns a value of type @a@.
-- The @s@ parameter is either
--
-- * an unstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'Control.Monad.ST.stToIO').
--
-- It serves to keep the internal states of different invocations
-- of 'runST' separate from each other and from invocations of
-- 'Control.Monad.ST.stToIO'.
newtype ST s a = ST (STRep s a)
type STRep s a = State# s -> (# State# s, a #)

instance Functor (ST s) where
    fmap f (ST m) = ST $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST (\ s -> (# s, x #))
    m >> k   = m >>= \ _ -> k

    (ST m) >>= k
      = ST (\ s ->
	case (m s) of { (# new_s, r #) ->
	case (k r) of { ST k2 ->
	(k2 new_s) }})

data STret s a = STret (State# s) a

-- liftST is useful when we want a lifted result from an ST computation.  See
-- fixST below.
liftST :: ST s a -> State# s -> STret s a
liftST (ST m) = \s -> case m s of (# s', r #) -> STret s' r

{-# NOINLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST ( \ s ->
    let
	r = case m s of (# _, res #) -> res
    in
    (# s, r #)
  )

-- | Allow the result of a state transformer computation to be used (lazily)
-- inside the computation.
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let ans       = liftST (k r) s
	STret _ r = ans
    in
    case ans of STret s' x -> (# s', x #)

instance  Show (ST s a)  where
    showsPrec _ _  = showString "<<ST action>>"
    showList	   = showList__ (showsPrec 0)
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
{-# INLINE runST #-}
-- The INLINE prevents runSTRep getting inlined in *this* module
-- so that it is still visible when runST is inlined in an importing
-- module.  Regrettably delicate.  runST is behaving like a wrapper.

-- | Return the value computed by a state transformer computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
runST st = runSTRep (case st of { ST st_rep -> st_rep })

-- I'm only letting runSTRep be inlined right at the end, in particular *after* full laziness
-- That's what the "INLINE [0]" says.
-- 		SLPJ Apr 99
{-# INLINE [0] runSTRep #-}
runSTRep :: (forall s. STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
	      		(# _, r #) -> r
\end{code}
