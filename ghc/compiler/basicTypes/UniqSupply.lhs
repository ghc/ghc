%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[UniqSupply]{The @UniqueSupply@ data type and a (monadic) supply thereof}

\begin{code}
#include "HsVersions.h"

module UniqSupply (

	UniqSupply,		-- Abstractly

	getUnique, getUniques,	-- basic ops

	UniqSM(..),		-- type: unique supply monad
	initUs, thenUs, returnUs,
	mapUs, mapAndUnzipUs, mapAndUnzip3Us,
	thenMaybeUs, mapAccumLUs,

	mkSplitUniqSupply,
	splitUniqSupply,

	-- and the access functions for the `builtin' UniqueSupply
	getBuiltinUniques, mkBuiltinUnique,
	mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3
  ) where

import Ubiq{-uitous-}

import Unique
import Util

import PreludeGlaST

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x :: Int#)
\end{code}


%************************************************************************
%*									*
\subsection{Splittable Unique supply: @UniqSupply@}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection[UniqSupply-type]{@UniqSupply@ type and operations}
%*									*
%************************************************************************

A value of type @UniqSupply@ is unique, and it can
supply {\em one} distinct @Unique@.  Also, from the supply, one can
also manufacture an arbitrary number of further @UniqueSupplies@,
which will be distinct from the first and from all others.

\begin{code}
data UniqSupply
  = MkSplitUniqSupply Int	-- make the Unique with this
		   UniqSupply UniqSupply
				-- when split => these two supplies
\end{code}

\begin{code}
mkSplitUniqSupply :: Char -> PrimIO UniqSupply

splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
getUnique :: UniqSupply -> Unique
getUniques :: Int -> UniqSupply -> [Unique]
\end{code}

\begin{code}
mkSplitUniqSupply (MkChar c#)
  = let
	mask# = (i2w (ord# c#)) `shiftL#` (i2w_s 24#)

	-- here comes THE MAGIC:

	mk_supply#
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
getUnique (MkSplitUniqSupply (MkInt n) _ _) = mkUniqueGrimily n

getUniques i@(MkInt i#) supply = i# `get_from` supply
  where
    get_from 0# _ = []
    get_from n# (MkSplitUniqSupply (MkInt u#) _ s2)
      = mkUniqueGrimily u# : get_from (n# `minusInt#` 1#) s2
\end{code}

%************************************************************************
%*									*
\subsubsection[UniqSupply-monad]{@UniqSupply@ monad: @UniqSM@}
%*									*
%************************************************************************

\begin{code}
type UniqSM result = UniqSupply -> result

-- the initUs function also returns the final UniqSupply

initUs :: UniqSupply -> UniqSM a -> (UniqSupply, a)

initUs init_us m
  = case (splitUniqSupply init_us) of { (s1, s2) ->
    (s2, m s1) }

{-# INLINE thenUs #-}
{-# INLINE returnUs #-}
{-# INLINE splitUniqSupply #-}
\end{code}

@thenUs@ is where we split the @UniqSupply@.
\begin{code}
thenUs :: UniqSM a -> (a -> UniqSM b) -> UniqSM b

thenUs expr cont us
  = case (splitUniqSupply us) of { (s1, s2) ->
    case (expr s1)	      of { result ->
    cont result s2 }}
\end{code}

\begin{code}
returnUs :: a -> UniqSM a
returnUs result us = result

mapUs :: (a -> UniqSM b) -> [a] -> UniqSM [b]

mapUs f []     = returnUs []
mapUs f (x:xs)
  = f x         `thenUs` \ r  ->
    mapUs f xs  `thenUs` \ rs ->
    returnUs (r:rs)

mapAndUnzipUs  :: (a -> UniqSM (b,c))   -> [a] -> UniqSM ([b],[c])
mapAndUnzip3Us :: (a -> UniqSM (b,c,d)) -> [a] -> UniqSM ([b],[c],[d])

mapAndUnzipUs f [] = returnUs ([],[])
mapAndUnzipUs f (x:xs)
  = f x		    	`thenUs` \ (r1,  r2)  ->
    mapAndUnzipUs f xs	`thenUs` \ (rs1, rs2) ->
    returnUs (r1:rs1, r2:rs2)

mapAndUnzip3Us f [] = returnUs ([],[],[])
mapAndUnzip3Us f (x:xs)
  = f x		    	`thenUs` \ (r1,  r2,  r3)  ->
    mapAndUnzip3Us f xs	`thenUs` \ (rs1, rs2, rs3) ->
    returnUs (r1:rs1, r2:rs2, r3:rs3)

thenMaybeUs :: UniqSM (Maybe a) -> (a -> UniqSM (Maybe b)) -> UniqSM (Maybe b)
thenMaybeUs m k
  = m	`thenUs` \ result ->
    case result of
      Nothing -> returnUs Nothing
      Just x  -> k x

mapAccumLUs :: (acc -> x -> UniqSM (acc, y))
	    -> acc
	    -> [x]
	    -> UniqSM (acc, [y])

mapAccumLUs f b []     = returnUs (b, [])
mapAccumLUs f b (x:xs)
  = f b x   	        	    `thenUs` \ (b__2, x__2) ->
    mapAccumLUs f b__2 xs   	    `thenUs` \ (b__3, xs__2) ->
    returnUs (b__3, x__2:xs__2)
\end{code}

%************************************************************************
%*									*
\subsubsection[UniqueSupplies-compiler]{@UniqueSupplies@ specific to the compiler}
%*									*
%************************************************************************

\begin{code}
mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
 mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUnique1 i = mkUnique 'C' i -- used for getItsUnique on Regs
mkPseudoUnique2 i = mkUnique 'D' i -- ditto
mkPseudoUnique3 i = mkUnique 'E' i -- ditto

getBuiltinUniques :: Int -> [Unique]
getBuiltinUniques n = map (mkUnique 'B') [1 .. n]
\end{code}

The following runs a uniq monad expression, using builtin uniq values:
\begin{code}
--runBuiltinUs :: UniqSM a -> a
--runBuiltinUs m = snd (initUs uniqSupply_B m)
\end{code}
