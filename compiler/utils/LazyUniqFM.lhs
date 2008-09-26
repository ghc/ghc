%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

LazyUniqFM: Specialised lazy finite maps, for things with @Uniques@

Based on @UniqFM@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

\begin{code}
module LazyUniqFM (
	-- * Lazy unique-keyed mappings
	UniqFM,   	-- abstract type

	-- ** Manipulating those mappings
	emptyUFM,
	unitUFM,
	unitDirectlyUFM,
	listToUFM,
	listToUFM_Directly,
	addToUFM,addToUFM_C,addToUFM_Acc,
	addListToUFM,addListToUFM_C,
	addToUFM_Directly,
	addListToUFM_Directly,
	delFromUFM,
	delFromUFM_Directly,
	delListFromUFM,
	plusUFM,
	plusUFM_C,
	minusUFM,
	intersectsUFM,
	intersectUFM,
	intersectUFM_C,
	foldUFM, foldUFM_Directly,
	mapUFM,
	elemUFM, elemUFM_Directly,
	filterUFM, filterUFM_Directly,
	sizeUFM,
	hashUFM,
	isNullUFM,
	lookupUFM, lookupUFM_Directly,
	lookupWithDefaultUFM, lookupWithDefaultUFM_Directly,
	eltsUFM, keysUFM,
	ufmToList 
    ) where

import qualified UniqFM as S

import Unique
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{The @UniqFM@ type, and signatures for the functions}
%*									*
%************************************************************************

We use @FiniteMaps@, with a (@getUnique@-able) @Unique@ as ``key''.

\begin{code}
emptyUFM	:: UniqFM elt
isNullUFM	:: UniqFM elt -> Bool
unitUFM		:: Uniquable key => key -> elt -> UniqFM elt
unitDirectlyUFM -- got the Unique already
		:: Unique -> elt -> UniqFM elt
listToUFM	:: Uniquable key => [(key,elt)] -> UniqFM elt
listToUFM_Directly
		:: [(Unique, elt)] -> UniqFM elt

addToUFM	:: Uniquable key => UniqFM elt -> key -> elt  -> UniqFM elt
addListToUFM	:: Uniquable key => UniqFM elt -> [(key,elt)] -> UniqFM elt
addListToUFM_Directly :: UniqFM elt -> [(Unique,elt)] -> UniqFM elt
addToUFM_Directly
		:: UniqFM elt -> Unique -> elt -> UniqFM elt

addToUFM_C	:: Uniquable key => (elt -> elt -> elt)	-- old -> new -> result
			   -> UniqFM elt 		-- old
			   -> key -> elt 		-- new
			   -> UniqFM elt		-- result

addToUFM_Acc	:: Uniquable key =>
			      (elt -> elts -> elts)	-- Add to existing
			   -> (elt -> elts)		-- New element
			   -> UniqFM elts 		-- old
			   -> key -> elt 		-- new
			   -> UniqFM elts		-- result

addListToUFM_C	:: Uniquable key => (elt -> elt -> elt)
			   -> UniqFM elt -> [(key,elt)]
			   -> UniqFM elt

delFromUFM	:: Uniquable key => UniqFM elt -> key	 -> UniqFM elt
delListFromUFM	:: Uniquable key => UniqFM elt -> [key] -> UniqFM elt
delFromUFM_Directly :: UniqFM elt -> Unique -> UniqFM elt

plusUFM		:: UniqFM elt -> UniqFM elt -> UniqFM elt

plusUFM_C	:: (elt -> elt -> elt)
		-> UniqFM elt -> UniqFM elt -> UniqFM elt

minusUFM	:: UniqFM elt1 -> UniqFM elt2 -> UniqFM elt1

intersectUFM	:: UniqFM elt -> UniqFM elt -> UniqFM elt
intersectUFM_C	:: (elt1 -> elt2 -> elt3)
		-> UniqFM elt1 -> UniqFM elt2 -> UniqFM elt3
intersectsUFM	:: UniqFM elt1 -> UniqFM elt2 -> Bool

foldUFM		:: (elt -> a -> a) -> a -> UniqFM elt -> a
foldUFM_Directly:: (Unique -> elt -> a -> a) -> a -> UniqFM elt -> a
mapUFM		:: (elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
filterUFM	:: (elt -> Bool) -> UniqFM elt -> UniqFM elt
filterUFM_Directly :: (Unique -> elt -> Bool) -> UniqFM elt -> UniqFM elt

sizeUFM		:: UniqFM elt -> Int
hashUFM		:: UniqFM elt -> Int
elemUFM		:: Uniquable key => key -> UniqFM elt -> Bool
elemUFM_Directly:: Unique -> UniqFM elt -> Bool

lookupUFM	:: Uniquable key => UniqFM elt -> key -> Maybe elt
lookupUFM_Directly  -- when you've got the Unique already
		:: UniqFM elt -> Unique -> Maybe elt
lookupWithDefaultUFM
		:: Uniquable key => UniqFM elt -> elt -> key -> elt
lookupWithDefaultUFM_Directly
		:: UniqFM elt -> elt -> Unique -> elt

keysUFM		:: UniqFM elt -> [Unique]	-- Get the keys
eltsUFM		:: UniqFM elt -> [elt]
ufmToList	:: UniqFM elt -> [(Unique, elt)]
\end{code}

%************************************************************************
%*									*
\subsection{The @IdFinMap@ and @TyVarFinMap@ specialisations for Ids/TyVars}
%*									*
%************************************************************************

\begin{code}
-- Turn off for now, these need to be updated (SDM 4/98)

#if 0
#ifdef __GLASGOW_HASKELL__
-- I don't think HBC was too happy about this (WDP 94/10)

{-# SPECIALIZE
    addListToUFM :: UniqFM elt -> [(Name,   elt)] -> UniqFM elt
  #-}
{-# SPECIALIZE
    addListToUFM_C :: (elt -> elt -> elt) -> UniqFM elt -> [(Name,  elt)] -> UniqFM elt
  #-}
{-# SPECIALIZE
    addToUFM	:: UniqFM elt -> Unique -> elt  -> UniqFM elt
  #-}
{-# SPECIALIZE
    listToUFM	:: [(Unique, elt)]     -> UniqFM elt
  #-}
{-# SPECIALIZE
    lookupUFM	:: UniqFM elt -> Name   -> Maybe elt
		 , UniqFM elt -> Unique -> Maybe elt
  #-}

#endif /* __GLASGOW_HASKELL__ */
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ type, and signatures for the functions}
%*									*
%************************************************************************

@UniqFM a@ is a mapping from Unique to a.

\begin{code}
data Lazy a = Lazy { fromLazy :: a }

-- | @UniqFM a@ is a mapping from Unique to @a@ where the element @a@ is evaluated lazily.
newtype UniqFM ele = MkUniqFM (S.UniqFM (Lazy ele))

instance Outputable a => Outputable (UniqFM a) where
    ppr (MkUniqFM fm) = ppr fm

instance Outputable a => Outputable (Lazy a) where
    ppr (Lazy x) = ppr x
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ functions}
%*									*
%************************************************************************

First the ways of building a UniqFM.

\begin{code}
emptyUFM		     = MkUniqFM $ S.EmptyUFM
unitUFM	     key elt = MkUniqFM $ S.unitUFM key (Lazy elt)
unitDirectlyUFM key elt = MkUniqFM $ S.unitDirectlyUFM key (Lazy elt)

listToUFM key_elt_pairs
    = MkUniqFM $ S.listToUFM [ (k, Lazy v) | (k, v) <- key_elt_pairs ]
listToUFM_Directly uniq_elt_pairs
    = MkUniqFM
    $ S.listToUFM_Directly [ (k, Lazy v) | (k, v) <- uniq_elt_pairs ]
\end{code}

Now ways of adding things to UniqFMs.

There is an alternative version of @addListToUFM_C@, that uses @plusUFM@,
but the semantics of this operation demands a linear insertion;
perhaps the version without the combinator function
could be optimised using it.

\begin{code}
addToUFM (MkUniqFM fm) key elt = MkUniqFM $ S.addToUFM fm key (Lazy elt)

addToUFM_Directly (MkUniqFM fm) u elt
    = MkUniqFM $ S.addToUFM_Directly fm u (Lazy elt)

addToUFM_C combiner (MkUniqFM fm) key elt
  = MkUniqFM $ S.addToUFM_C combiner' fm key (Lazy elt)
    where combiner' (Lazy l) (Lazy r) = Lazy (combiner l r)

addToUFM_Acc add unit (MkUniqFM fm) key item
    = MkUniqFM $ S.addToUFM_Acc add' unit' fm key item
    where add' elt (Lazy elts) = Lazy (add elt elts)
          unit' elt = Lazy (unit elt)

addListToUFM (MkUniqFM fm) key_elt_pairs
    = MkUniqFM $ S.addListToUFM fm [ (k, Lazy v) | (k, v) <- key_elt_pairs ]
addListToUFM_Directly (MkUniqFM fm) uniq_elt_pairs
    = MkUniqFM
    $ S.addListToUFM_Directly fm [ (k, Lazy v) | (k, v) <- uniq_elt_pairs ]

addListToUFM_C combiner (MkUniqFM fm) key_elt_pairs
 = MkUniqFM
 $ S.addListToUFM_C combiner' fm [ (k, Lazy v) | (k, v) <- key_elt_pairs ]
    where combiner' (Lazy l) (Lazy r) = Lazy (combiner l r)
\end{code}

Now ways of removing things from UniqFM.

\begin{code}
delListFromUFM (MkUniqFM fm) lst = MkUniqFM $ S.delListFromUFM fm lst

delFromUFM          (MkUniqFM fm) key = MkUniqFM $ S.delFromUFM          fm key
delFromUFM_Directly (MkUniqFM fm) u   = MkUniqFM $ S.delFromUFM_Directly fm u
\end{code}

Now ways of adding two UniqFM's together.

\begin{code}
plusUFM (MkUniqFM tr1) (MkUniqFM tr2) = MkUniqFM $ S.plusUFM tr1 tr2

plusUFM_C f (MkUniqFM tr1) (MkUniqFM tr2) = MkUniqFM $ S.plusUFM_C f' tr1 tr2
    where f' (Lazy l) (Lazy r) = Lazy $ f l r
\end{code}

And ways of subtracting them. First the base cases,
then the full D&C approach.

\begin{code}
minusUFM (MkUniqFM fm1) (MkUniqFM fm2) = MkUniqFM $ S.minusUFM fm1 fm2
\end{code}

And taking the intersection of two UniqFM's.

\begin{code}
intersectUFM  (MkUniqFM t1) (MkUniqFM t2) = MkUniqFM $ S.intersectUFM t1 t2
intersectsUFM (MkUniqFM t1) (MkUniqFM t2) = S.intersectsUFM t1 t2

intersectUFM_C f (MkUniqFM fm1) (MkUniqFM fm2)
    = MkUniqFM $ S.intersectUFM_C f' fm1 fm2
    where f' (Lazy l) (Lazy r) = Lazy $ f l r
\end{code}

Now the usual set of `collection' operators, like map, fold, etc.

\begin{code}
foldUFM f a (MkUniqFM ufm) = S.foldUFM f' a ufm
    where f' (Lazy elt) x = f elt x
\end{code}

\begin{code}
mapUFM fn (MkUniqFM fm) = MkUniqFM (S.mapUFM fn' fm)
    where fn' (Lazy elt) = Lazy (fn elt)

filterUFM fn (MkUniqFM fm) = MkUniqFM (S.filterUFM fn' fm)
    where fn' (Lazy elt) = fn elt

filterUFM_Directly fn (MkUniqFM fm) = MkUniqFM $ S.filterUFM_Directly fn' fm
    where fn' u (Lazy elt) = fn u elt
\end{code}

Note, this takes a long time, O(n), but
because we dont want to do this very often, we put up with this.
O'rable, but how often do we look at the size of
a finite map?

\begin{code}
sizeUFM (MkUniqFM fm) = S.sizeUFM fm

isNullUFM (MkUniqFM fm) = S.isNullUFM fm

-- hashing is used in VarSet.uniqAway, and should be fast
-- We use a cheap and cheerful method for now
hashUFM (MkUniqFM fm) = S.hashUFM fm
\end{code}

looking up in a hurry is the {\em whole point} of this binary tree lark.
Lookup up a binary tree is easy (and fast).

\begin{code}
elemUFM          key (MkUniqFM fm) = S.elemUFM          key fm
elemUFM_Directly key (MkUniqFM fm) = S.elemUFM_Directly key fm

lookupUFM (MkUniqFM fm) key = fmap fromLazy $ S.lookupUFM fm key
lookupUFM_Directly (MkUniqFM fm) key
    = fmap fromLazy $ S.lookupUFM_Directly fm key

lookupWithDefaultUFM (MkUniqFM fm) deflt key
    = fromLazy $ S.lookupWithDefaultUFM fm (Lazy deflt) key

lookupWithDefaultUFM_Directly (MkUniqFM fm) deflt key
 = fromLazy $ S.lookupWithDefaultUFM_Directly fm (Lazy deflt) key
\end{code}

folds are *wonderful* things.

\begin{code}
eltsUFM   (MkUniqFM fm) = map fromLazy $ S.eltsUFM fm
keysUFM   (MkUniqFM fm) = S.keysUFM fm
ufmToList (MkUniqFM fm) = [ (k, v) | (k, Lazy v) <- S.ufmToList fm ]
foldUFM_Directly f elt (MkUniqFM fm)
    = S.foldUFM_Directly f' elt fm
    where f' u (Lazy elt') x = f u elt' x
\end{code}

