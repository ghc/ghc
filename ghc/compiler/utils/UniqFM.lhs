%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[UniqFM]{Specialised finite maps, for things with @Uniques@}

Based on @FiniteMaps@ (as you would expect).

Basically, the things need to be in class @NamedThing@, and we use the
@getTheUnique@ method to grab their @Uniques@.

(A similar thing to @UniqSet@, as opposed to @Set@.)

@IdEnv@ and @TyVarEnv@ are the (backward-compatible?) specialisations
of this stuff for Ids and TyVars, respectively.

\begin{code}
#if defined(COMPILING_GHC)
#include "HsVersions.h"
#define IF_NOT_GHC(a) {--}
#else
#define ASSERT(e) {--}
#define IF_NOT_GHC(a) a
#endif

module UniqFM (
	UniqFM,   -- abstract type

	emptyUFM,
	singletonUFM,
	singletonDirectlyUFM,
	listToUFM,
	listToUFM_Directly,
	addToUFM,
	IF_NOT_GHC(addListToUFM COMMA)
	addToUFM_Directly,
	IF_NOT_GHC(addToUFM_C COMMA)
	IF_NOT_GHC(addListToUFM_C COMMA)
	delFromUFM,
	delListFromUFM,
	plusUFM,
	plusUFM_C,
	minusUFM,
	intersectUFM,
	IF_NOT_GHC(intersectUFM_C COMMA)
	IF_NOT_GHC(foldUFM COMMA)
	mapUFM,
	filterUFM,
	sizeUFM,
	isNullUFM,
	lookupUFM,
	lookupDirectlyUFM,
	IF_NOT_GHC(lookupWithDefaultUFM COMMA)
	eltsUFM,
	ufmToList,

	-- to make the interface self-sufficient
	Id, TyVar, Unique
	IF_ATTACK_PRAGMAS(COMMA u2i)	-- profiling
    ) where

import AbsUniType	-- for specialisation to TyVars
import Id		-- for specialisation to Ids
import IdInfo		-- sigh
import Maybes		( maybeToBool, Maybe(..) )
import Name
import Outputable
import Unique		( u2i, mkUniqueGrimily, Unique )
import Util
#if ! OMIT_NATIVE_CODEGEN
import AsmRegAlloc	( Reg )
#define IF_NCG(a) a
#else
#define IF_NCG(a) {--}
#endif
\end{code}

%************************************************************************
%*									*
\subsection{The @UniqFM@ type, and signatures for the functions}
%*									*
%************************************************************************

We use @FiniteMaps@, with a (@getTheUnique@-able) @Unique@ as ``key''.

\begin{code}
emptyUFM	:: UniqFM elt
isNullUFM	:: UniqFM elt -> Bool
singletonUFM	:: NamedThing key => key -> elt -> UniqFM elt
singletonDirectlyUFM -- got the Unique already
		:: Unique -> elt -> UniqFM elt
listToUFM	:: NamedThing key => [(key,elt)] -> UniqFM elt
listToUFM_Directly
		:: [(Unique, elt)] -> UniqFM elt

addToUFM	:: NamedThing key => UniqFM elt -> key -> elt  -> UniqFM elt
addListToUFM	:: NamedThing key => UniqFM elt -> [(key,elt)] -> UniqFM elt
addToUFM_Directly
		:: UniqFM elt -> Unique -> elt -> UniqFM elt

addToUFM_C	:: NamedThing key => (elt -> elt -> elt)
			   -> UniqFM elt -> key -> elt -> UniqFM elt
addListToUFM_C	:: NamedThing key => (elt -> elt -> elt)
			   -> UniqFM elt -> [(key,elt)]
			   -> UniqFM elt

delFromUFM	:: NamedThing key => UniqFM elt -> key	 -> UniqFM elt
delListFromUFM	:: NamedThing key => UniqFM elt -> [key] -> UniqFM elt

plusUFM		:: UniqFM elt -> UniqFM elt -> UniqFM elt

plusUFM_C	:: (elt -> elt -> elt)
		-> UniqFM elt -> UniqFM elt -> UniqFM elt

minusUFM	:: UniqFM elt -> UniqFM elt -> UniqFM elt

intersectUFM	:: UniqFM elt -> UniqFM elt -> UniqFM elt
intersectUFM_C	:: (elt -> elt -> elt)
		-> UniqFM elt -> UniqFM elt -> UniqFM elt
foldUFM		:: (elt -> a -> a) -> a -> UniqFM elt -> a
mapUFM		:: (elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
filterUFM	:: (elt -> Bool) -> UniqFM elt -> UniqFM elt

sizeUFM		:: UniqFM elt -> Int

lookupUFM	:: NamedThing key => UniqFM elt -> key -> Maybe elt
lookupDirectlyUFM  -- when you've got the Unique already
		:: UniqFM elt -> Unique -> Maybe elt
lookupWithDefaultUFM
		:: NamedThing key => UniqFM elt -> elt -> key -> elt

eltsUFM		:: UniqFM elt -> [elt]
ufmToList	:: UniqFM elt -> [(Unique, elt)]
\end{code}

%************************************************************************
%*									*
\subsection{The @IdFinMap@ and @TyVarFinMap@ specialisations for Ids/TyVars}
%*									*
%************************************************************************

\begin{code}
type IdFinMap	 elt = UniqFM elt
type TyVarFinMap elt = UniqFM elt
type NameFinMap  elt = UniqFM elt
type RegFinMap   elt = UniqFM elt
\end{code}

\begin{code}
#ifdef __GLASGOW_HASKELL__
-- I don't think HBC was too happy about this (WDP 94/10)

{-# SPECIALIZE
    singletonUFM :: Id	  -> elt -> IdFinMap elt,
		    TyVar -> elt -> TyVarFinMap elt,
		    Name  -> elt -> NameFinMap elt
    IF_NCG(COMMA    Reg   -> elt -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    listToUFM	:: [(Id,   elt)]     -> IdFinMap elt,
		   [(TyVar,elt)]     -> TyVarFinMap elt,
		   [(Name, elt)]     -> NameFinMap elt
    IF_NCG(COMMA   [(Reg COMMA elt)] -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    addToUFM	:: IdFinMap    elt -> Id    -> elt  -> IdFinMap elt,
		   TyVarFinMap elt -> TyVar -> elt  -> TyVarFinMap elt,
		   NameFinMap  elt -> Name  -> elt  -> NameFinMap elt
    IF_NCG(COMMA   RegFinMap   elt -> Reg   -> elt  -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    addListToUFM :: IdFinMap	elt -> [(Id,   elt)] -> IdFinMap elt,
		    TyVarFinMap elt -> [(TyVar,elt)] -> TyVarFinMap elt,
		    NameFinMap  elt -> [(Name,elt)]  -> NameFinMap elt
    IF_NCG(COMMA    RegFinMap   elt -> [(Reg COMMA elt)] -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    addToUFM_C	:: (elt -> elt -> elt)
		-> IdFinMap elt -> Id -> elt -> IdFinMap elt,
		   (elt -> elt -> elt)
		-> TyVarFinMap elt -> TyVar -> elt -> TyVarFinMap elt,
		   (elt -> elt -> elt)
		-> NameFinMap elt -> Name -> elt -> NameFinMap elt
    IF_NCG(COMMA   (elt -> elt -> elt)
    		-> RegFinMap elt -> Reg -> elt -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    addListToUFM_C :: (elt -> elt -> elt)
		-> IdFinMap elt -> [(Id,elt)] -> IdFinMap elt,
		   (elt -> elt -> elt)
		-> TyVarFinMap elt -> [(TyVar,elt)] -> TyVarFinMap elt,
		   (elt -> elt -> elt)
		-> NameFinMap elt -> [(Name,elt)] -> NameFinMap elt
    IF_NCG(COMMA   (elt -> elt -> elt)
		-> RegFinMap elt -> [(Reg COMMA elt)] -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    delFromUFM	:: IdFinMap elt	   -> Id    -> IdFinMap elt,
		   TyVarFinMap elt -> TyVar -> TyVarFinMap elt,
		   NameFinMap elt  -> Name  -> NameFinMap elt
    IF_NCG(COMMA    RegFinMap elt   -> Reg   -> RegFinMap elt)
  #-}
{-# SPECIALIZE
    delListFromUFM :: IdFinMap elt    -> [Id]	 -> IdFinMap elt,
		      TyVarFinMap elt -> [TyVar] -> TyVarFinMap elt,
		      NameFinMap elt  -> [Name]  -> NameFinMap elt
    IF_NCG(COMMA      RegFinMap elt   -> [Reg]   -> RegFinMap elt)
  #-}

{-# SPECIALIZE
    lookupUFM	:: IdFinMap elt	   -> Id    -> Maybe elt,
		   TyVarFinMap elt -> TyVar -> Maybe elt,
		   NameFinMap elt  -> Name  -> Maybe elt
    IF_NCG(COMMA   RegFinMap elt   -> Reg   -> Maybe elt)
  #-}
{-# SPECIALIZE
    lookupWithDefaultUFM
		:: IdFinMap elt	   -> elt -> Id	   -> elt,
		   TyVarFinMap elt -> elt -> TyVar -> elt,
		   NameFinMap elt  -> elt -> Name  -> elt
    IF_NCG(COMMA   RegFinMap elt   -> elt -> Reg   -> elt)
  #-}

#endif {- __GLASGOW_HASKELL__ -}
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
  | LeafUFM FAST_INT ele
  | NodeUFM FAST_INT	    -- the switching
	    FAST_INT	    -- the delta
	    (UniqFM ele)
	    (UniqFM ele)

-- for debugging only :-)
{-
instance Text (UniqFM a) where
	showsPrec _ (NodeUFM a b t1 t2) =
		  showString "NodeUFM " . shows (IBOX(a))
		. showString " " . shows (IBOX(b))
		. showString " (" . shows t1
		. showString ") (" . shows t2
		. showString ")"
	showsPrec _ (LeafUFM x a) = showString "LeafUFM " . shows (IBOX(x))
	showsPrec _ (EmptyUFM) = id
-}
\end{code}

%************************************************************************
%*									*
\subsubsection{The @UniqFM@ functions}
%*									*
%************************************************************************

First the ways of building a UniqFM.

\begin{code}
emptyUFM		     = EmptyUFM
singletonUFM	     key elt = mkLeafUFM (u2i (getTheUnique key)) elt
singletonDirectlyUFM key elt = mkLeafUFM (u2i key) elt

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

addToUFM_Directly fm u elt = insert_ele use_snd fm (u2i u) elt

addToUFM_C combiner fm key elt
  = insert_ele combiner fm (u2i (getTheUnique key)) elt

addListToUFM fm key_elt_pairs = addListToUFM_C use_snd fm key_elt_pairs

addListToUFM_C combiner fm key_elt_pairs
 = foldl (\ fm (k, e) -> insert_ele combiner fm (u2i (getTheUnique k)) e)
	 fm key_elt_pairs

addListToUFM_directly_C combiner fm uniq_elt_pairs
 = foldl (\ fm (k, e) -> insert_ele combiner fm (u2i k) e)
	 fm uniq_elt_pairs
\end{code}

Now ways of removing things from UniqFM.

\begin{code}
delListFromUFM fm lst = foldl delFromUFM fm lst

delFromUFM fm key = delete fm (u2i (getTheUnique key))

delete EmptyUFM _   = EmptyUFM
delete fm       key = del_ele fm
  where
    del_ele :: UniqFM a -> UniqFM a

    del_ele lf@(LeafUFM j _)
      | j _EQ_ key	= EmptyUFM
      | otherwise	= lf	-- no delete!

    del_ele nd@(NodeUFM j p t1 t2)
      | j _GT_ key
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
	  mix_branches (LeftRoot Left) -- | trace "LL" True
	    = mkSLNodeUFM
		(NodeUFMData j p)
		(mix_trees t1 right_t)	
		t2

	  mix_branches (LeftRoot Right) -- | trace "LR" True
	    = mkLSNodeUFM
		(NodeUFMData j p)
		t1
		(mix_trees t2 right_t)	

	  mix_branches (RightRoot Left) -- | trace "RL" True
	    = mkSLNodeUFM
		(NodeUFMData j' p')
		(mix_trees left_t t1')	
		t2'

	  mix_branches (RightRoot Right) -- | trace "RR" True
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
	minus_trees lf@(LeafUFM i a) t2	       =
		case lookup t2 i of
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
	  minus_branches (LeftRoot Left)
	    = mkSLNodeUFM
		(NodeUFMData j p)
		(minus_trees t1 right_t)	
		t2
	  minus_branches (LeftRoot Right)
	    = mkLSNodeUFM
		(NodeUFMData j p)
		t1
		(minus_trees t2 right_t)	

		--
		-- The right is above the left
		--
	  minus_branches (RightRoot Left)
	    = minus_trees left_t t1'
	  minus_branches (RightRoot Right)
	    = minus_trees left_t t2'

	minus_trees _ _ = panic "EmptyUFM found when insering into plusInt"
\end{code}

And taking the intersection of two UniqFM's.

\begin{code}
intersectUFM t1 t2 = intersectUFM_C use_snd t1 t2

intersectUFM_C f EmptyUFM _ = EmptyUFM
intersectUFM_C f _ EmptyUFM = EmptyUFM
intersectUFM_C f fm1 fm2    = intersect_trees fm1 fm2
    where
	intersect_trees (LeafUFM i a) t2 =
		case lookup t2 i of
		  Nothing -> EmptyUFM
		  Just b -> mkLeafUFM i (f a b)

	intersect_trees t1 (LeafUFM i a) =
		case lookup t1 i of
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
					
	  intersect_branches (LeftRoot Left)
	    = intersect_trees t1 right_t
	  intersect_branches (LeftRoot Right)
	    = intersect_trees t2 right_t
	  intersect_branches (RightRoot Left)
	    = intersect_trees left_t t1'
	  intersect_branches (RightRoot Right)
	    = intersect_trees left_t t2'

	intersect_trees x y = panic ("EmptyUFM found when intersecting trees")
\end{code}

Now the usual set of `collection' operators, like map, fold, etc.

\begin{code}
foldUFM fn a EmptyUFM = a
foldUFM fn a fm	      = fold_tree fn a fm

mapUFM fn EmptyUFM    = EmptyUFM
mapUFM fn fm	      = map_tree fn fm

filterUFM fn EmptyUFM = EmptyUFM
filterUFM fn fm	      = filter_tree fn fm
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
\end{code}

looking up in a hurry is the {\em whole point} of this binary tree lark.
Lookup up a binary tree is easy (and fast).

\begin{code}
lookupUFM	  fm key = lookup fm (u2i (getTheUnique key))
lookupDirectlyUFM fm key = lookup fm (u2i key)

lookupWithDefaultUFM fm deflt key
  = case lookup fm (u2i (getTheUnique key)) of
      Nothing  -> deflt
      Just elt -> elt

lookup EmptyUFM _   = Nothing
lookup fm i	    = lookup_tree fm
  where
	lookup_tree :: UniqFM a -> Maybe a

	lookup_tree (LeafUFM j b)
	  | j _EQ_ i	= Just b
	  | otherwise	= Nothing
	lookup_tree (NodeUFM j p t1 t2)
	  | j _GT_ i	= lookup_tree t1
	  | otherwise	= lookup_tree t2

	lookup_tree EmptyUFM = panic "lookup Failed"
\end{code}

folds are *wonderful* things.

\begin{code}
eltsUFM EmptyUFM = []
eltsUFM fm       = fold_tree (:) [] fm

ufmToList EmptyUFM = []
ufmToList fm
  = fold_tree (\ iu elt rest -> (mkUniqueGrimily iu, elt) : rest) [] fm
  where
    fold_tree f a (NodeUFM _ _ t1 t2) = fold_tree f (fold_tree f a t2) t1
    fold_tree f a (LeafUFM iu obj)    = f iu obj a

    fold_tree f a EmptyUFM = panic "Should Never fold over an EmptyUFM"
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
mkLeafUFM :: FAST_INT -> a -> UniqFM a
mkLeafUFM i a	  = LeafUFM i a

-- The *ONLY* ways of building a NodeUFM.

mkSSNodeUFM (NodeUFMData j p) EmptyUFM t2 = t2
mkSSNodeUFM (NodeUFMData j p) t1 EmptyUFM = t1
mkSSNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (IBOX(j)) (IBOX(p)) t1 t2)
    NodeUFM j p t1 t2

mkSLNodeUFM (NodeUFMData j p) EmptyUFM t2 = t2
mkSLNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (IBOX(j)) (IBOX(p)) t1 t2)
    NodeUFM j p t1 t2

mkLSNodeUFM (NodeUFMData j p) t1 EmptyUFM = t1
mkLSNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (IBOX(j)) (IBOX(p)) t1 t2)
    NodeUFM j p t1 t2

mkLLNodeUFM (NodeUFMData j p) t1 t2
  = ASSERT(correctNodeUFM (IBOX(j)) (IBOX(p)) t1 t2)
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
      = low <= IBOX(i) && IBOX(i) <= high
    correct low high above_p (NodeUFM j p _ _)
      = low <= IBOX(j) && IBOX(j) <= high && above_p > IBOX(p)
    correct _ _ _ EmptyUFM = panic "EmptyUFM stored inside a tree"
\end{code}

Note: doing SAT on this by hand seems to make it worse. Todo: Investigate,
and if necessary do $\lambda$ lifting on our functions that are bound.

\begin{code}
insert_ele
	:: (a -> a -> a)
	-> UniqFM a
	-> FAST_INT
	-> a
	-> UniqFM a

insert_ele f EmptyUFM i new = mkLeafUFM i new

insert_ele f (LeafUFM j old) i new
  | j _GT_ i =
	  mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  (indexToRoot j))
		 (mkLeafUFM i new)
		 (mkLeafUFM j old)
  | j _EQ_ i  = mkLeafUFM j (f old new)
  | otherwise =
	  mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  (indexToRoot j))
		 (mkLeafUFM j old)
		 (mkLeafUFM i new)

insert_ele f n@(NodeUFM j p t1 t2) i a
  | i _LT_ j
    = if (i _GE_ (j _SUB_ p))
      then mkSLNodeUFM (NodeUFMData j p) (insert_ele f t1 i a) t2
      else mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  ((NodeUFMData j p)))
		  (mkLeafUFM i a)
		  n
  | otherwise
    = if (i _LE_ ((j _SUB_ ILIT(1)) _ADD_ p))
      then mkLSNodeUFM (NodeUFMData j p) t1 (insert_ele f t2 i a)
      else mkLLNodeUFM (getCommonNodeUFMData
			  (indexToRoot i)
			  ((NodeUFMData j p)))
		  n
		  (mkLeafUFM i a)
\end{code}

This has got a left to right ordering.

\begin{code}
fold_tree f a (NodeUFM _ _ t1 t2) = fold_tree f (fold_tree f a t2) t1
fold_tree f a (LeafUFM _ obj)	  = f obj a

fold_tree f a EmptyUFM = panic "Should Never fold over an EmptyUFM"
\end{code}

\begin{code}
map_tree f (NodeUFM j p t1 t2)
  = mkSSNodeUFM (NodeUFMData j p) (map_tree f t1) (map_tree f t2)
map_tree f (LeafUFM i obj)
  = mkLeafUFM i (f obj)

map_tree f _ = panic "map_tree failed"
\end{code}

\begin{code}
filter_tree f nd@(NodeUFM j p t1 t2)
  = mkSSNodeUFM (NodeUFMData j p) (filter_tree f t1) (filter_tree f t2)

filter_tree f lf@(LeafUFM i obj)	
  | f obj = lf
  | otherwise = EmptyUFM
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
  = NodeUFMData FAST_INT
		FAST_INT
\end{code}

This is the information used when computing new NodeUFMs.

\begin{code}
data Side = Left | Right
data CommonRoot
  = LeftRoot  Side	-- which side is the right down ?
  | RightRoot Side	-- which side is the left down ?
  | SameRoot		-- they are the same !
  | NewRoot NodeUFMData	-- here's the new, common, root
	    Bool	-- do you need to swap left and right ?
\end{code}

This specifies the relationship between NodeUFMData and CalcNodeUFMData.

\begin{code}
indexToRoot :: FAST_INT -> NodeUFMData

indexToRoot i
  = let
	l = (ILIT(1) :: FAST_INT)
    in
    NodeUFMData (((i `shiftR_` l) `shiftL_` l) _ADD_ ILIT(1)) l

getCommonNodeUFMData :: NodeUFMData -> NodeUFMData -> NodeUFMData

getCommonNodeUFMData (NodeUFMData i p) (NodeUFMData i2 p2)
  | p _EQ_ p2	= getCommonNodeUFMData_ p j j2
  | p _LT_ p2	= getCommonNodeUFMData_ p2 (j _QUOT_ (p2 _QUOT_ p)) j2
  | otherwise	= getCommonNodeUFMData_ p j (j2 _QUOT_ (p _QUOT_ p2))
  where
    l  = (ILIT(1) :: FAST_INT)
    j  = i  _QUOT_ (p  `shiftL_` l)
    j2 = i2 _QUOT_ (p2 `shiftL_` l)

    getCommonNodeUFMData_ :: FAST_INT -> FAST_INT -> FAST_INT -> NodeUFMData

    getCommonNodeUFMData_ p j j_
      | j _EQ_ j_
      = NodeUFMData (((j `shiftL_` l) _ADD_ l) _MUL_ p) p
      | otherwise
      = getCommonNodeUFMData_ (p `shiftL_`  l) (j `shiftR_` l) (j_ `shiftR_` l)

ask_about_common_ancestor :: NodeUFMData -> NodeUFMData -> CommonRoot

ask_about_common_ancestor x@(NodeUFMData j p) y@(NodeUFMData j2 p2)
  | j _EQ_ j2 = SameRoot
  | otherwise
  = case getCommonNodeUFMData x y of
      nd@(NodeUFMData j3 p3)
	| j3 _EQ_ j  -> LeftRoot (decideSide (j _GT_ j2))
	| j3 _EQ_ j2 -> RightRoot (decideSide (j _LT_ j2))
	| otherwise   -> NewRoot nd (j _GT_ j2)
    where
	decideSide :: Bool -> Side
	decideSide True	 = Left
	decideSide False = Right
\end{code}

This might be better in Util.lhs ?


Now the bit twiddling functions.
\begin{code}
shiftL_ :: FAST_INT -> FAST_INT -> FAST_INT
shiftR_ :: FAST_INT -> FAST_INT -> FAST_INT

#if __GLASGOW_HASKELL__
{-# INLINE shiftL_ #-}
{-# INLINE shiftR_ #-}
shiftL_ n p = word2Int#((int2Word# n) `shiftL#` p)
shiftR_ n p = word2Int#((int2Word# n) `shiftr` p)
# if __GLASGOW_HASKELL__ >= 23
  where
    shiftr x y = shiftRA# x y
# else
    shiftr x y = shiftR#  x y
# endif

#else {- not GHC -}
shiftL_ n p = n * (2 ^ p)
shiftR_ n p = n `quot` (2 ^ p)

#endif {- not GHC -}
\end{code}

Andy's extras: ToDo: to Util.

\begin{code}
use_fst :: a -> b -> a
use_fst a b = a

use_snd :: a -> b -> b
use_snd a b = b
\end{code}
