%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[Unique]{The @SplitUniqSupply@ data type (``splittable Unique supply'')}

\begin{code}
#include "HsVersions.h"

module SplitUniq (
	SplitUniqSupply,		-- abstract types

	getSUnique, getSUniques,	-- basic ops
	getSUniqueAndDepleted, getSUniquesAndDepleted,	-- DEPRECATED!

	SUniqSM(..),		-- type: unique supply monad
	initSUs, thenSUs, returnSUs,
	mapSUs, mapAndUnzipSUs,

	mkSplitUniqSupply,
	splitUniqSupply,

	-- to make interface self-sufficient
	Unique
	IF_ATTACK_PRAGMAS(COMMA mkUniqueGrimily)

#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

import Outputable	-- class for printing, forcing
import Pretty		-- pretty-printing utilities
import PrimOps		-- ** DIRECTLY **
import Unique
import Util

#if defined(__HBC__)
{-hide import from mkdependHS-}
import
	Word
import
	NameSupply	renaming ( Name to HBC_Name )
#endif
#ifdef __GLASGOW_HASKELL__
# if __GLASGOW_HASKELL__ >= 26
import PreludeGlaST
# else
import PreludePrimIO
import PreludeGlaST	( unsafeInterleaveST
			  IF_ATTACK_PRAGMAS(COMMA fixST)
			)
# endif
#endif

infixr 9 `thenUs`

#ifdef __GLASGOW_HASKELL__
w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x :: Int#)
#endif
\end{code}

%************************************************************************
%*									*
\subsection[SplitUniqSupply-type]{@SplitUniqSupply@ type and operations}
%*									*
%************************************************************************

A value of type @SplitUniqSupply@ is unique, and it can
supply {\em one} distinct @Unique@.  Also, from the supply, one can
also manufacture an arbitrary number of further @UniqueSupplies@,
which will be distinct from the first and from all others.

Common type signatures
\begin{code}
-- mkSplitUniqSupply :: differs by implementation!

splitUniqSupply :: SplitUniqSupply -> (SplitUniqSupply, SplitUniqSupply)
getSUnique :: SplitUniqSupply -> Unique
getSUniques :: Int -> SplitUniqSupply -> [Unique]
getSUniqueAndDepleted :: SplitUniqSupply -> (Unique, SplitUniqSupply)
getSUniquesAndDepleted :: Int -> SplitUniqSupply -> ([Unique], SplitUniqSupply)
\end{code}

%************************************************************************
%*									*
\subsubsection{Chalmers implementation of @SplitUniqSupply@}
%*									*
%************************************************************************

\begin{code}
#if defined(__HBC__)

data SplitUniqSupply = MkSplit NameSupply

mkSplitUniqSupply :: Char -> SplitUniqSupply -- NB: not the same type

mkSplitUniqSupply _ = MkSplit initialNameSupply

splitUniqSupply (MkSplit us)
  = case (splitNameSupply us) of { (s1, s2) ->
    (MkSplit s1, MkSplit s2) }

getSUnique supply = error "getSUnique" -- mkUniqueGrimily (getName supply)

getSUniques i supply
  = error "getSUniques" -- [ mkUniqueGrimily (getName s) | s <- take i (listNameSupply supply) ]

getSUniqueAndDepleted supply
  = error "getSUniqueAndDepleted"
{-
    let
	u = mkUniqueGrimily (getName supply)
	(s1, _) = splitNameSupply supply
    in
    (u, s1)
-}

getSUniquesAndDepleted i supply
  = error "getSUniquesAndDepleted"
{-
  = let
	supplies = take (i+1) (listNameSupply supply)
	uniqs	 = [ mkUniqueGrimily (getName s) | s <- take i supplies ]
	last_supply = drop i supplies
    in
    (uniqs, last_supply)
-}

#endif {- end of Chalmers implementation -}
\end{code}

%************************************************************************
%*									*
\subsubsection{Glasgow implementation of @SplitUniqSupply@}
%*									*
%************************************************************************

Glasgow Haskell implementation:
\begin{code}
#ifdef __GLASGOW_HASKELL__

# ifdef IGNORE_REFERENTIAL_TRANSPARENCY

data SplitUniqSupply = MkSplitUniqSupply {-does nothing-}

mkSplitUniqSupply :: Char -> PrimIO SplitUniqSupply
mkSplitUniqSupply (MkChar c#) = returnPrimIO MkSplitUniqSupply

splitUniqSupply _ = (MkSplitUniqSupply, MkSplitUniqSupply)

getSUnique s = unsafe_mk_unique s

getSUniques i@(MkInt i#) supply = get_from i# supply
  where
    get_from 0# s = []
    get_from n# s
      = unsafe_mk_unique s : get_from (n# `minusInt#` 1#) s

getSUniqueAndDepleted s = (unsafe_mk_unique s, MkSplitUniqSupply)

getSUniquesAndDepleted i@(MkInt i#) s = get_from [] i# s
  where
    get_from acc 0# s = (acc, MkSplitUniqSupply)
    get_from acc n# s
      = get_from (unsafe_mk_unique s : acc) (n# `minusInt#` 1#) s

unsafe_mk_unique supply -- this is the TOTALLY unacceptable bit
  = unsafePerformPrimIO (
    _ccall_ genSymZh junk	`thenPrimIO` \ (W# u#) ->
    returnPrimIO (mkUniqueGrimily (w2i (mask# `or#` u#)))
    )
  where
    mask# = (i2w (ord# 'x'#)) `shiftL#` (i2w_s 24#)
    junk  = case supply of { MkSplitUniqSupply -> (1::Int) }

# else {- slight attention to referential transparency -}

data SplitUniqSupply
  = MkSplitUniqSupply Int	-- make the Unique with this
		   SplitUniqSupply SplitUniqSupply
				-- when split => these two supplies
\end{code}

@mkSplitUniqSupply@ is used to get a @SplitUniqSupply@ started.
\begin{code}

mkSplitUniqSupply :: Char -> PrimIO SplitUniqSupply

-- ToDo: 64-bit bugs here!!???

mkSplitUniqSupply (MkChar c#)
  = let
	mask# = (i2w (ord# c#)) `shiftL#` (i2w_s 24#)

	-- here comes THE MAGIC:

	mk_supply#
{- OLD:
	  = unsafe_interleave mk_unique	 `thenPrimIO` \ uniq ->
	    unsafe_interleave mk_supply# `thenPrimIO` \ s1 ->
	    unsafe_interleave mk_supply# `thenPrimIO` \ s2 ->
	    returnPrimIO (MkSplitUniqSupply uniq s1 s2)
-}
	  = unsafe_interleave (
		mk_unique   `thenPrimIO` \ uniq ->
		mk_supply#  `thenPrimIO` \ s1 ->
		mk_supply#  `thenPrimIO` \ s2 ->
		returnPrimIO (MkSplitUniqSupply uniq s1 s2)
	    )
	  where
	    -- inlined copy of unsafeInterleavePrimIO;
	    -- this is the single-most-hammered bit of code
	    -- in the compiler....
	    unsafe_interleave m s
	      = let
		    (r, new_s) = m s
		in
		(r, s)

        mk_unique = _ccall_ genSymZh		`thenPrimIO` \ (W# u#) ->
		    returnPrimIO (MkInt (w2i (mask# `or#` u#)))
    in
    mk_supply#

splitUniqSupply (MkSplitUniqSupply _ s1 s2) = (s1, s2)
\end{code}

\begin{code}
getSUnique (MkSplitUniqSupply (MkInt n) _ _) = mkUniqueGrimily n

getSUniques i@(MkInt i#) supply = i# `get_from` supply
  where
    get_from 0# _ = []
    get_from n# (MkSplitUniqSupply (MkInt u#) _ s2)
      = mkUniqueGrimily u# : get_from (n# `minusInt#` 1#) s2

getSUniqueAndDepleted (MkSplitUniqSupply (MkInt n) s1 _) = (mkUniqueGrimily n, s1)

getSUniquesAndDepleted i@(MkInt i#) supply = get_from [] i# supply
  where
    get_from acc 0# s = (acc, s)
    get_from acc n# (MkSplitUniqSupply (MkInt u#) _ s2)
      = get_from (mkUniqueGrimily u# : acc) (n# `minusInt#` 1#) s2

# endif {- slight attention to referential transparency -}

#endif  {- end of Glasgow implementation -}
\end{code}

%************************************************************************
%*									*
\subsection[SplitUniq-monad]{Splittable Unique-supply monad}
%*									*
%************************************************************************

\begin{code}
type SUniqSM result = SplitUniqSupply -> result

-- the initUs function also returns the final SplitUniqSupply

initSUs :: SplitUniqSupply -> SUniqSM a -> (SplitUniqSupply, a)

initSUs init_us m
  = case (splitUniqSupply init_us) of { (s1, s2) ->
    (s2, m s1) }

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenSUs #-}
{-# INLINE returnSUs #-}
{-# INLINE splitUniqSupply #-}
#endif
\end{code}

@thenSUs@ is where we split the @SplitUniqSupply@.
\begin{code}
thenSUs :: SUniqSM a -> (a -> SUniqSM b) -> SUniqSM b

thenSUs expr cont us
  = case (splitUniqSupply us) of { (s1, s2) ->
    case (expr s1)	      of { result ->
    cont result s2 }}
\end{code}

\begin{code}
returnSUs :: a -> SUniqSM a
returnSUs result us = result

mapSUs :: (a -> SUniqSM b) -> [a] -> SUniqSM [b]

mapSUs f []     = returnSUs []
mapSUs f (x:xs)
  = f x         `thenSUs` \ r  ->
    mapSUs f xs  `thenSUs` \ rs ->
    returnSUs (r:rs)

mapAndUnzipSUs  :: (a -> SUniqSM (b,c))   -> [a] -> SUniqSM ([b],[c])

mapAndUnzipSUs f [] = returnSUs ([],[])
mapAndUnzipSUs f (x:xs)
  = f x		    	`thenSUs` \ (r1,  r2)  ->
    mapAndUnzipSUs f xs	`thenSUs` \ (rs1, rs2) ->
    returnSUs (r1:rs1, r2:rs2)
\end{code}
