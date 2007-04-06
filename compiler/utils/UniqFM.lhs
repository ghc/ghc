%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

UniqFM: Specialised finite maps, for things with @Uniques@

Based on @FiniteMaps@ (as you would expect).

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

(A similar thing to @UniqSet@, as opposed to @Set@.)

\begin{code}
module UniqFM (
	UniqFM,   -- abstract type

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
	foldUFM,
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

#include "HsVersions.h"

import Unique		( Uniquable(..), Unique, getKey#, mkUniqueGrimily )
import Maybes		( maybeToBool )
import FastTypes
import Outputable

import GHC.Exts		-- Lots of Int# operations
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
\subsection{Andy Gill's underlying @UniqFM@ machinery}
%*									*
%************************************************************************

``Uniq Finite maps'' are the heart and soul of the compiler's
lookup-tables/environments.  Important stuff!  It works well with
Dense and Sparse ranges.
Both @Uq@ Finite maps and @Hash@ Finite Maps
are built ontop of Int Finite Maps.

This code is explained in the paper:
\begin{display}
	A Gill, S Peyton Jones, B O'Sullivan, W Partain and Aqua Friends
	"A Cheap balancing act that grows on a tree"
	Glasgow FP Workshop, Sep 1994, pp??-??
\end{display}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ type, and signatures for the functions}
%*									*
%************************************************************************

@UniqFM a@ is a mapping from Unique to a.

First, the DataType itself; which is either a Node, a Leaf, or an Empty.

\begin{code}
data UniqFM ele
  = EmptyUFM
  | LeafUFM FastInt ele
  | NodeUFM FastInt	    -- the switching
	    FastInt	    -- the delta
	    (UniqFM ele)
	    (UniqFM ele)
-- INVARIANT: the children of a NodeUFM are never EmptyUFMs

{-
-- for debugging only :-)
instance Outputable (UniqFM a) where
	ppr(NodeUFM a b t1 t2) =
		sep [text "NodeUFM " <+> int IBOX(a) <+> int IBOX(b),
		     nest 1 (parens (ppr t1)),
		     nest 1 (parens (ppr t2))]
	ppr (LeafUFM x a) = text "LeafUFM " <+> int IBOX(x)
	ppr (EmptyUFM)    = empty
-}
-- and when not debugging the package itself...
instance Outputable a => Outputable (UniqFM a) where
    ppr ufm = ppr (ufmToList ufm)
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ functions}
%*									*
%************************************************************************

First the ways of building a UniqFM.

\begin{code}
emptyUFM		     = EmptyUFM
unitUFM	     key elt = mkLeafUFM (getKey# (getUnique key)) elt
unitDirectlyUFM key elt = mkLeafUFM (getKey# key) elt

listToUFM key_elt_pairs
  = addListToUFM_C use_snd EmptyUFM key_elt_pairs

listToUFM_Directly uniq_elt_pairs
  = addListToUFM_directly_C use_snd EmptyUFM uniq_elt_pairs
\end{code}

Now ways of adding things to UniqFMs.

There is an alternative version of @addListToUFM_C@, that uses @plusUFM@,
but the semantics of this operation demands a linear insertion;
perhaps the version without the combinator function
could be optimised using it.

\begin{code}
addToUFM fm key elt = addToUFM_C use_snd fm key elt

addToUFM_Directly fm u elt = insert_ele use_snd fm (getKey# u) elt

addToUFM_C combiner fm key elt
  = insert_ele combiner fm (getKey# (getUnique key)) elt

addToUFM_Acc add unit fm key item
  = insert_ele combiner fm (getKey# (getUnique key)) (unit item)
  where
    combiner old _unit_item = add item old

addListToUFM fm key_elt_pairs = addListToUFM_C use_snd fm key_elt_pairs
addListToUFM_Directly fm uniq_elt_pairs = addListToUFM_directly_C use_snd fm uniq_elt_pairs

addListToUFM_C combiner fm key_elt_pairs
 = foldl (\ fm (k, e) -> insert_ele combiner fm (getKey# (getUnique k)) e)
	 fm key_elt_pairs

addListToUFM_directly_C combiner fm uniq_elt_pairs
 = foldl (\ fm (k, e) -> insert_ele combiner fm (getKey# k) e)
	 fm uniq_elt_pairs
\end{code}

Now ways of removing things from UniqFM.

\begin{code}
delListFromUFM fm lst = foldl delFromUFM fm lst

delFromUFM          fm key = delete fm (getKey# (getUnique key))
delFromUFM_Directly fm u   = delete fm (getKey# u)

delete EmptyUFM _   = EmptyUFM
delete fm       key = del_ele fm
  where
    del_ele :: UniqFM a -> UniqFM a

    del_ele lf@(LeafUFM j _)
      | j ==# key	= EmptyUFM
      | otherwise	= lf	-- no delete!

    del_ele nd@(NodeUFM j p t1 t2)
      | j ># key
      = mkSLNodeUFM (NodeUFMData j p) (del_ele t1) t2
      | otherwise
      = mkLSNodeUFM (NodeUFMData j p) t1 (del_ele t2)

    del_ele _ = panic "Found EmptyUFM FM when rec-deleting"
\end{code}

Now ways of adding two UniqFM's together.

\begin{code}
plusUFM tr1 tr2 = plusUFM_C use_snd tr1 tr2

plusUFM_C f EmptyUFM tr	= tr
plusUFM_C f tr EmptyUFM	= tr
plusUFM_C f fm1 fm2	= mix_trees fm1 fm2
    where
	mix_trees (LeafUFM i a) t2 = insert_ele (flip f) t2 i a
	mix_trees t1 (LeafUFM i a) = insert_ele f	 t1 i a

	mix_trees left_t@(NodeUFM j p t1 t2) right_t@(NodeUFM j' p' t1' t2')
	  = mix_branches
		(ask_about_common_ancestor
			(NodeUFMData j p)
			(NodeUFMData j' p'))
	  where
		-- Given a disjoint j,j' (p >^ p' && p' >^ p):
		--
		--	  j		j'			(C j j')
		--	 / \	+      / \	==>		/	\
		--     t1   t2	    t1'	  t2'		       j	 j'
		--					      / \	/ \
		--					     t1	 t2   t1'  t2'
		-- Fast, Ehh !
		--
	  mix_branches (NewRoot nd False)
		= mkLLNodeUFM nd left_t right_t
	  mix_branches (NewRoot nd True)
		= mkLLNodeUFM nd right_t left_t

		-- Now, if j == j':
		--
		--	  j		j'			 j
		--	 / \	+      / \	==>		/ \
		--     t1   t2	    t1'	  t2'		t1 + t1'   t2 + t2'
		--
	  mix_branches (SameRoot)
		= mkSSNodeUFM (NodeUFMData j p)
			(mix_trees t1 t1')
			(mix_trees t2 t2')
		-- Now the 4 different other ways; all like this:
		--
		-- Given j >^ j' (and, say,  j > j')
		--
		--	  j		j'			 j
		--	 / \	+      / \	==>		/ \
		--     t1   t2	    t1'	  t2'		      t1   t2 + j'
		--						       / \
		--						     t1'  t2'
	  mix_branches (LeftRoot Leftt) --  | trace "LL" True
	    = mkSLNodeUFM
		(NodeUFMData j p)
		(mix_trees t1 right_t)
		t2

	  mix_branches (LeftRoot Rightt) --  | trace "LR" True
	    = mkLSNodeUFM
		(NodeUFMData j p)
		t1
		(mix_trees t2 right_t)

	  mix_branches (RightRoot Leftt) --  | trace "RL" True
	    = mkSLNodeUFM
		(NodeUFMData j' p')
		(mix_trees left_t t1')
		t2'

	  mix_branches (RightRoot Rightt) --  | trace "RR" True
	    = mkLSNodeUFM
		(NodeUFMData j' p')
		t1'
		(mix_trees left_t t2')

	mix_trees _ _ = panic "EmptyUFM found when inserting into plusInt"
\end{code}

And ways of subtracting them. First the base cases,
then the full D&C approach.

\begin{code}
minusUFM EmptyUFM _  = EmptyUFM
minusUFM t1 EmptyUFM = t1
minusUFM fm1 fm2     = minus_trees fm1 fm2
    where
	--
	-- Notice the asymetry of subtraction
	--
	minus_trees lf@(LeafUFM i a) t2 =
		case lookUp t2 i of
		  Nothing -> lf
		  Just b -> EmptyUFM

	minus_trees t1 (LeafUFM i _) = delete t1 i

	minus_trees left_t@(NodeUFM j p t1 t2) right_t@(NodeUFM j' p' t1' t2')
	  = minus_branches
		(ask_about_common_ancestor
			(NodeUFMData j p)
			(NodeUFMData j' p'))
	  where
		-- Given a disjoint j,j' (p >^ p' && p' >^ p):
		--
		--	  j		j'		   j
		--	 / \	+      / \	==>	  / \
		--     t1   t2	    t1'	  t2'		 t1  t2
		--
		--
		-- Fast, Ehh !
		--
	  minus_branches (NewRoot nd _) = left_t

		-- Now, if j == j':
		--
		--	  j		j'			 j
		--	 / \	+      / \	==>		/ \
		--     t1   t2	    t1'	  t2'		t1 + t1'   t2 + t2'
		--
	  minus_branches (SameRoot)
		= mkSSNodeUFM (NodeUFMData j p)
			(minus_trees t1 t1')
			(minus_trees t2 t2')
		-- Now the 4 different other ways; all like this:
		-- again, with asymatry

		--
		-- The left is above the right
		--
	  minus_branches (LeftRoot Leftt)
	    = mkSLNodeUFM
		(NodeUFMData j p)
		(minus_trees t1 right_t)
		t2
	  minus_branches (LeftRoot Rightt)
	    = mkLSNodeUFM
		(NodeUFMData j p)
		t1
		(minus_trees t2 right_t)

		--
		-- The right is above the left
		--
	  minus_branches (RightRoot Leftt)
	    = minus_trees left_t t1'
	  minus_branches (RightRoot Rightt)
	    = minus_trees left_t t2'

	minus_trees _ _ = panic "EmptyUFM found when insering into plusInt"
\end{code}

And taking the intersection of two UniqFM's.

\begin{code}
intersectUFM  t1 t2 = intersectUFM_C use_snd t1 t2
intersectsUFM t1 t2 = isNullUFM (intersectUFM_C (\ _ _ -> error "urk") t1 t2)

intersectUFM_C f EmptyUFM _ = EmptyUFM
intersectUFM_C f _ EmptyUFM = EmptyUFM
intersectUFM_C f fm1 fm2    = intersect_trees fm1 fm2
    where
	intersect_trees (LeafUFM i a) t2 =
		case lookUp t2 i of
		  Nothing -> EmptyUFM
		  Just b -> mkLeafUFM i (f a b)

	intersect_trees t1 (LeafUFM i a) =
		case lookUp t1 i of
		  Nothing -> EmptyUFM
		  Just b -> mkLeafUFM i (f b a)

	intersect_trees left_t@(NodeUFM j p t1 t2) right_t@(NodeUFM j' p' t1' t2')
	  = intersect_branches
		(ask_about_common_ancestor
			(NodeUFMData j p)
			(NodeUFMData j' p'))
	  where
		-- Given a disjoint j,j' (p >^ p' && p' >^ p):
		--
		--	  j		j'
		--	 / \	+      / \	==>		EmptyUFM
		--     t1   t2	    t1'	  t2'
		--
		-- Fast, Ehh !
		--
	  intersect_branches (NewRoot nd _) = EmptyUFM

		-- Now, if j == j':
		--
		--	  j		j'			 j
		--	 / \	+      / \	==>		/ \
		--     t1   t2	    t1'	  t2'		t1 x t1'   t2 x t2'
		--
	  intersect_branches (SameRoot)
		= mkSSNodeUFM (NodeUFMData j p)
			(intersect_trees t1 t1')
			(intersect_trees t2 t2')
		-- Now the 4 different other ways; all like this:
		--
		-- Given j >^ j' (and, say,  j > j')
		--
		--	  j		j'		       t2 + j'
		--	 / \	+      / \	==>		   / \
		--     t1   t2	    t1'	  t2'			 t1'  t2'
		--
		-- This does cut down the search space quite a bit.

	  intersect_branches (LeftRoot Leftt)
	    = intersect_trees t1 right_t
	  intersect_branches (LeftRoot Rightt)
	    = intersect_trees t2 right_t
	  intersect_branches (RightRoot Leftt)
	    = intersect_trees left_t t1'
	  intersect_branches (RightRoot Rightt)
	    = intersect_trees left_t t2'

	intersect_trees x y = panic ("EmptyUFM found when intersecting trees")
\end{code}

Now the usual set of `collection' operators, like map, fold, etc.

\begin{code}
foldUFM f a (NodeUFM _ _ t1 t2) = foldUFM f (foldUFM f a t2) t1
foldUFM f a (LeafUFM _ obj)     = f obj a
foldUFM f a EmptyUFM	        = a
\end{code}

\begin{code}
mapUFM fn EmptyUFM    = EmptyUFM
mapUFM fn fm	      = map_tree fn fm

filterUFM fn EmptyUFM = EmptyUFM
filterUFM fn fm	      = filter_tree pred fm
	where
	  pred (i::FastInt) e = fn e

filterUFM_Directly fn EmptyUFM = EmptyUFM
filterUFM_Directly fn fm       = filter_tree pred fm
	where
	  pred i e = fn (mkUniqueGrimily (iBox i)) e
\end{code}

Note, this takes a long time, O(n), but
because we dont want to do this very often, we put up with this.
O'rable, but how often do we look at the size of
a finite map?

\begin{code}
sizeUFM EmptyUFM	    = 0
sizeUFM (NodeUFM _ _ t1 t2) = sizeUFM t1 + sizeUFM t2
sizeUFM (LeafUFM _ _)	    = 1

isNullUFM EmptyUFM = True
isNullUFM _	   = False

-- hashing is used in VarSet.uniqAway, and should be fast
-- We use a cheap and cheerful method for now
hashUFM EmptyUFM          = 0
hashUFM (NodeUFM n _ _ _) = iBox n
hashUFM (LeafUFM n _)     = iBox n
\end{code}

looking up in a hurry is the {\em whole point} of this binary tree lark.
Lookup up a binary tree is easy (and fast).

\begin{code}
elemUFM          key fm = maybeToBool (lookupUFM fm key)
elemUFM_Directly key fm = maybeToBool (lookupUFM_Directly fm key)

lookupUFM	   fm key = lookUp fm (getKey# (getUnique key))
lookupUFM_Directly fm key = lookUp fm (getKey# key)

lookupWithDefaultUFM fm deflt key
  = case lookUp fm (getKey# (getUnique key)) of
      Nothing  -> deflt
      Just elt -> elt

lookupWithDefaultUFM_Directly fm deflt key
  = case lookUp fm (getKey# key) of
      Nothing  -> deflt
      Just elt -> elt

lookUp EmptyUFM _   = Nothing
lookUp fm i	    = lookup_tree fm
  where
	lookup_tree :: UniqFM a -> Maybe a

	lookup_tree (LeafUFM j b)
	  | j ==# i	= Just b
	  | otherwise	= Nothing
	lookup_tree (NodeUFM j p t1 t2)
	  | j ># i	= lookup_tree t1
	  | otherwise	= lookup_tree t2

	lookup_tree EmptyUFM = panic "lookup Failed"
\end{code}

folds are *wonderful* things.

\begin{code}
eltsUFM fm = foldUFM (:) [] fm

ufmToList fm = fold_tree (\ iu elt rest -> (mkUniqueGrimily (iBox iu), elt) : rest) [] fm

keysUFM fm = fold_tree (\ iu elt rest -> mkUniqueGrimily (iBox iu) : rest) [] fm

fold_tree f a (NodeUFM _ _ t1 t2) = fold_tree f (fold_tree f a t2) t1
fold_tree f a (LeafUFM iu obj)    = f iu obj a
fold_tree f a EmptyUFM		  = a
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ type, and its functions}
%*									*
%************************************************************************

You should always use these to build the tree.
There are 4 versions of mkNodeUFM, depending on
the strictness of the two sub-tree arguments.
The strictness is used *both* to prune out
empty trees, *and* to improve performance,
stoping needless thunks lying around.
The rule of thumb (from experence with these trees)
is make thunks strict, but data structures lazy.
If in doubt, use mkSSNodeUFM, which has the `strongest'
functionality, but may do a few needless evaluations.

\begin{code}
mkLeafUFM :: FastInt -> a -> UniqFM a
mkLeafUFM i a	  = LeafUFM i a

-- The *ONLY* ways of building a NodeUFM.

mkSSNodeUFM (NodeUFMData j p) EmptyUFM t2 = t2
mkSSNodeUFM (NodeUFMData j p) t1 EmptyUFM = t1
mkSSNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (iBox j) (iBox p) t1 t2)
    NodeUFM j p t1 t2

mkSLNodeUFM (NodeUFMData j p) EmptyUFM t2 = t2
mkSLNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (iBox j) (iBox p) t1 t2)
    NodeUFM j p t1 t2

mkLSNodeUFM (NodeUFMData j p) t1 EmptyUFM = t1
mkLSNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (iBox j) (iBox p) t1 t2)
    NodeUFM j p t1 t2

mkLLNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (iBox j) (iBox p) t1 t2)
    NodeUFM j p t1 t2

correctNodeUFM
	:: Int
	-> Int
	-> UniqFM a
	-> UniqFM a
	-> Bool

correctNodeUFM j p t1 t2
  = correct (j-p) (j-1) p t1 && correct j ((j-1)+p) p t2
  where
    correct low high _ (LeafUFM i _)
      = low <= iBox i && iBox i <= high
    correct low high above_p (NodeUFM j p _ _)
      = low <= iBox j && iBox j <= high && above_p > iBox p
    correct _ _ _ EmptyUFM = panic "EmptyUFM stored inside a tree"
\end{code}

Note: doing SAT on this by hand seems to make it worse. Todo: Investigate,
and if necessary do $\lambda$ lifting on our functions that are bound.

\begin{code}
insert_ele
	:: (a -> a -> a)	-- old -> new -> result
	-> UniqFM a
	-> FastInt
	-> a
	-> UniqFM a

insert_ele f EmptyUFM i new = mkLeafUFM i new

insert_ele f (LeafUFM j old) i new
  | j ># i =
	  mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  (indexToRoot j))
		 (mkLeafUFM i new)
		 (mkLeafUFM j old)
  | j ==# i  = mkLeafUFM j (f old new)
  | otherwise =
	  mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  (indexToRoot j))
		 (mkLeafUFM j old)
		 (mkLeafUFM i new)

insert_ele f n@(NodeUFM j p t1 t2) i a
  | i <# j
    = if (i >=# (j -# p))
      then mkSLNodeUFM (NodeUFMData j p) (insert_ele f t1 i a) t2
      else mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  ((NodeUFMData j p)))
		  (mkLeafUFM i a)
		  n
  | otherwise
    = if (i <=# ((j -# _ILIT(1)) +# p))
      then mkLSNodeUFM (NodeUFMData j p) t1 (insert_ele f t2 i a)
      else mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  ((NodeUFMData j p)))
		  n
		  (mkLeafUFM i a)
\end{code}



\begin{code}
map_tree f (NodeUFM j p t1 t2)
  = mkLLNodeUFM (NodeUFMData j p) (map_tree f t1) (map_tree f t2)
	-- NB. lazy! we know the tree is well-formed.
map_tree f (LeafUFM i obj)
  = mkLeafUFM i (f obj)
map_tree f _ = panic "map_tree failed"
\end{code}

\begin{code}
filter_tree :: (FastInt -> a -> Bool) -> UniqFM a -> UniqFM a
filter_tree f nd@(NodeUFM j p t1 t2)
  = mkSSNodeUFM (NodeUFMData j p) (filter_tree f t1) (filter_tree f t2)

filter_tree f lf@(LeafUFM i obj)
  | f i obj = lf
  | otherwise = EmptyUFM
filter_tree f _ = panic "filter_tree failed"
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ type, and signatures for the functions}
%*									*
%************************************************************************

Now some Utilities;

This is the information that is held inside a NodeUFM, packaged up for
consumer use.

\begin{code}
data NodeUFMData
  = NodeUFMData FastInt
		FastInt
\end{code}

This is the information used when computing new NodeUFMs.

\begin{code}
data Side = Leftt | Rightt -- NB: avoid 1.3 names "Left" and "Right"
data CommonRoot
  = LeftRoot  Side	-- which side is the right down ?
  | RightRoot Side	-- which side is the left down ?
  | SameRoot		-- they are the same !
  | NewRoot NodeUFMData	-- here's the new, common, root
	    Bool	-- do you need to swap left and right ?
\end{code}

This specifies the relationship between NodeUFMData and CalcNodeUFMData.

\begin{code}
indexToRoot :: FastInt -> NodeUFMData

indexToRoot i
  = let
	l = (_ILIT(1) :: FastInt)
    in
    NodeUFMData (((i `shiftR_` l) `shiftL_` l) +# _ILIT(1)) l

getCommonNodeUFMData :: NodeUFMData -> NodeUFMData -> NodeUFMData

getCommonNodeUFMData (NodeUFMData i p) (NodeUFMData i2 p2)
  | p ==# p2	= getCommonNodeUFMData_ p j j2
  | p <# p2	= getCommonNodeUFMData_ p2 (j `quotFastInt` (p2 `quotFastInt` p)) j2
  | otherwise	= getCommonNodeUFMData_ p j (j2 `quotFastInt` (p `quotFastInt` p2))
  where
    l  = (_ILIT(1) :: FastInt)
    j  = i  `quotFastInt` (p  `shiftL_` l)
    j2 = i2 `quotFastInt` (p2 `shiftL_` l)

    getCommonNodeUFMData_ :: FastInt -> FastInt -> FastInt -> NodeUFMData

    getCommonNodeUFMData_ p j j_
      | j ==# j_
      = NodeUFMData (((j `shiftL_` l) +# l) *# p) p
      | otherwise
      = getCommonNodeUFMData_ (p `shiftL_`  l) (j `shiftR_` l) (j_ `shiftR_` l)

ask_about_common_ancestor :: NodeUFMData -> NodeUFMData -> CommonRoot

ask_about_common_ancestor x@(NodeUFMData j p) y@(NodeUFMData j2 p2)
  | j ==# j2 = SameRoot
  | otherwise
  = case getCommonNodeUFMData x y of
      nd@(NodeUFMData j3 p3)
	| j3 ==# j  -> LeftRoot (decideSide (j ># j2))
	| j3 ==# j2 -> RightRoot (decideSide (j <# j2))
	| otherwise   -> NewRoot nd (j ># j2)
    where
	decideSide :: Bool -> Side
	decideSide True	 = Leftt
	decideSide False = Rightt
\end{code}

This might be better in Util.lhs ?


Now the bit twiddling functions.
\begin{code}
shiftL_ :: FastInt -> FastInt -> FastInt
shiftR_ :: FastInt -> FastInt -> FastInt

#if __GLASGOW_HASKELL__
{-# INLINE shiftL_ #-}
{-# INLINE shiftR_ #-}
shiftL_ n p = word2Int#((int2Word# n) `uncheckedShiftL#` p)
shiftR_ n p = word2Int#((int2Word# n) `uncheckedShiftRL#` p)

#else /* not GHC */
shiftL_ n p = n * (2 ^ p)
shiftR_ n p = n `quot` (2 ^ p)

#endif /* not GHC */
\end{code}

\begin{code}
use_snd :: a -> b -> b
use_snd a b = b
\end{code}
