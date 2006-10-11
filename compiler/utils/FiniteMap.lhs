%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

``Finite maps'' are the heart of the compiler's
lookup-tables/environments and its implementation of sets.  Important
stuff!

This code is derived from that in the paper:
\begin{display}
	S Adams
	"Efficient sets: a balancing act"
	Journal of functional programming 3(4) Oct 1993, pp553-562
\end{display}

The code is SPECIALIZEd to various highly-desirable types (e.g., Id)
near the end.

\begin{code}

module FiniteMap (
	FiniteMap,		-- abstract type

	emptyFM, unitFM, listToFM,

	addToFM,
	addToFM_C,
	addListToFM,
	addListToFM_C,
	delFromFM,
	delListFromFM,

	plusFM,
	plusFM_C,
	minusFM,
	foldFM,

	intersectFM,
	intersectFM_C,
	mapFM, filterFM, 

	sizeFM, isEmptyFM, elemFM, lookupFM, lookupWithDefaultFM,

	fmToList, keysFM, eltsFM

	, bagToFM

    ) where

#include "HsVersions.h"
#define IF_NOT_GHC(a) {--}

#if defined(DEBUG_FINITEMAPS)/* NB NB NB */
#define OUTPUTABLE_key , Outputable key
#else
#define OUTPUTABLE_key {--}
#endif

import Maybes
import Bag	  ( Bag, foldrBag )
import Util
import Outputable

import GHC.Exts

#if ! OMIT_NATIVE_CODEGEN
#  define IF_NCG(a) a
#else
#  define IF_NCG(a) {--}
#endif


-- SIGH: but we use unboxed "sizes"...
#if __GLASGOW_HASKELL__
#define IF_GHC(a,b) a
#else /* not GHC */
#define IF_GHC(a,b) b
#endif /* not GHC */
\end{code}


%************************************************************************
%*									*
\subsection{The signature of the module}
%*									*
%************************************************************************

\begin{code}
--	BUILDING
emptyFM		:: FiniteMap key elt
unitFM		:: key -> elt -> FiniteMap key elt
listToFM	:: (Ord key OUTPUTABLE_key) => [(key,elt)] -> FiniteMap key elt
			-- In the case of duplicates, the last is taken
bagToFM		:: (Ord key OUTPUTABLE_key) => Bag (key,elt) -> FiniteMap key elt
			-- In the case of duplicates, who knows which is taken

--	ADDING AND DELETING
		   -- Throws away any previous binding
		   -- In the list case, the items are added starting with the
		   -- first one in the list
addToFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> elt  -> FiniteMap key elt
addListToFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

		   -- Combines with previous binding
		   -- The combining fn goes (old -> new -> new)
addToFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> key -> elt
			   -> FiniteMap key elt
addListToFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> [(key,elt)]
			   -> FiniteMap key elt

		   -- Deletion doesn't complain if you try to delete something
		   -- which isn't there
delFromFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key   -> FiniteMap key elt
delListFromFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> [key] -> FiniteMap key elt

--	COMBINING
		   -- Bindings in right argument shadow those in the left
plusFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt
			   -> FiniteMap key elt

		   -- Combines bindings for the same thing with the given function
plusFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

minusFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
		   -- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2

intersectFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersectFM_C	:: (Ord key OUTPUTABLE_key) => (elt1 -> elt2 -> elt3)
			   -> FiniteMap key elt1 -> FiniteMap key elt2 -> FiniteMap key elt3

--	MAPPING, FOLDING, FILTERING
foldFM		:: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
mapFM		:: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
filterFM	:: (Ord key OUTPUTABLE_key) => (key -> elt -> Bool)
			   -> FiniteMap key elt -> FiniteMap key elt


--	INTERROGATING
sizeFM		:: FiniteMap key elt -> Int
isEmptyFM	:: FiniteMap key elt -> Bool

elemFM		:: (Ord key OUTPUTABLE_key) => key -> FiniteMap key elt -> Bool
lookupFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> Maybe elt
lookupWithDefaultFM
		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> elt -> key -> elt
		-- lookupWithDefaultFM supplies a "default" elt
		-- to return for an unmapped key

--	LISTIFYING
fmToList	:: FiniteMap key elt -> [(key,elt)]
keysFM		:: FiniteMap key elt -> [key]
eltsFM		:: FiniteMap key elt -> [elt]
\end{code}

%************************************************************************
%*									*
\subsection{The @FiniteMap@ data type, and building of same}
%*									*
%************************************************************************

Invariants about @FiniteMap@:
\begin{enumerate}
\item
all keys in a FiniteMap are distinct
\item
all keys in left  subtree are $<$ key in Branch and
all keys in right subtree are $>$ key in Branch
\item
size field of a Branch gives number of Branch nodes in the tree
\item
size of left subtree is differs from size of right subtree by a
factor of at most \tr{sIZE_RATIO}
\end{enumerate}

\begin{code}
data FiniteMap key elt
  = EmptyFM
  | Branch key elt	    	-- Key and elt stored here
    IF_GHC(Int#,Int{-STRICT-})	-- Size >= 1
    (FiniteMap key elt)	    	-- Children
    (FiniteMap key elt)
\end{code}

\begin{code}
emptyFM = EmptyFM
{-
emptyFM
  = Branch bottom bottom IF_GHC(0#,0) bottom bottom
  where
    bottom = panic "emptyFM"
-}

--  #define EmptyFM (Branch _ _ IF_GHC(0#,0) _ _)

unitFM key elt = Branch key elt IF_GHC(1#,1) emptyFM emptyFM

listToFM = addListToFM emptyFM

bagToFM = foldrBag (\(k,v) fm -> addToFM fm k v) emptyFM
\end{code}

%************************************************************************
%*									*
\subsection{Adding to and deleting from @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
addToFM fm key elt = addToFM_C (\ old new -> new) fm key elt

addToFM_C combiner EmptyFM key elt = unitFM key elt
addToFM_C combiner (Branch key elt size fm_l fm_r) new_key new_elt
  = case compare new_key key of
	LT -> mkBalBranch key elt (addToFM_C combiner fm_l new_key new_elt) fm_r
	GT -> mkBalBranch key elt fm_l (addToFM_C combiner fm_r new_key new_elt)
	EQ -> Branch new_key (combiner elt new_elt) size fm_l fm_r

addListToFM fm key_elt_pairs = addListToFM_C (\ old new -> new) fm key_elt_pairs

addListToFM_C combiner fm key_elt_pairs
  = foldl' add fm key_elt_pairs	-- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C combiner fmap key elt
\end{code}

\begin{code}
delFromFM EmptyFM del_key = emptyFM
delFromFM (Branch key elt size fm_l fm_r) del_key
  = case compare del_key key of
	GT -> mkBalBranch key elt fm_l (delFromFM fm_r del_key)
	LT -> mkBalBranch key elt (delFromFM fm_l del_key) fm_r
	EQ -> glueBal fm_l fm_r

delListFromFM fm keys = foldl' delFromFM fm keys
\end{code}

%************************************************************************
%*									*
\subsection{Combining @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
plusFM_C combiner EmptyFM fm2 = fm2
plusFM_C combiner fm1 EmptyFM = fm1
plusFM_C combiner fm1 (Branch split_key elt2 _ left right)
  = mkVBalBranch split_key new_elt
		 (plusFM_C combiner lts left)
		 (plusFM_C combiner gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key
    new_elt = case lookupFM fm1 split_key of
		Nothing   -> elt2
		Just elt1 -> combiner elt1 elt2

-- It's worth doing plusFM specially, because we don't need
-- to do the lookup in fm1.
-- FM2 over-rides FM1.

plusFM EmptyFM fm2 = fm2
plusFM fm1 EmptyFM = fm1
plusFM fm1 (Branch split_key elt1 _ left right)
  = mkVBalBranch split_key elt1 (plusFM lts left) (plusFM gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key

minusFM EmptyFM fm2 = emptyFM
minusFM fm1 EmptyFM = fm1
minusFM fm1 (Branch split_key elt _ left right)
  = glueVBal (minusFM lts left) (minusFM gts right)
	-- The two can be way different, so we need glueVBal
  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

intersectFM fm1 fm2 = intersectFM_C (\ left right -> right) fm1 fm2

intersectFM_C combiner fm1 EmptyFM = emptyFM
intersectFM_C combiner EmptyFM fm2 = emptyFM
intersectFM_C combiner fm1 (Branch split_key elt2 _ left right)

  | maybeToBool maybe_elt1	-- split_elt *is* in intersection
  = mkVBalBranch split_key (combiner elt1 elt2) (intersectFM_C combiner lts left)
						(intersectFM_C combiner gts right)

  | otherwise			-- split_elt is *not* in intersection
  = glueVBal (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)

  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

    maybe_elt1 = lookupFM fm1 split_key
    Just elt1  = maybe_elt1
\end{code}

%************************************************************************
%*									*
\subsection{Mapping, folding, and filtering with @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
foldFM k z EmptyFM = z
foldFM k z (Branch key elt _ fm_l fm_r)
  = foldFM k (k key elt (foldFM k z fm_r)) fm_l

mapFM f EmptyFM = emptyFM
mapFM f (Branch key elt size fm_l fm_r)
  = Branch key (f key elt) size (mapFM f fm_l) (mapFM f fm_r)

filterFM p EmptyFM = emptyFM
filterFM p (Branch key elt _ fm_l fm_r)
  | p key elt		-- Keep the item
  = mkVBalBranch key elt (filterFM p fm_l) (filterFM p fm_r)

  | otherwise		-- Drop the item
  = glueVBal (filterFM p fm_l) (filterFM p fm_r)
\end{code}

%************************************************************************
%*									*
\subsection{Interrogating @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
--{-# INLINE sizeFM #-}
sizeFM EmptyFM		     = 0
sizeFM (Branch _ _ size _ _) = IF_GHC(I# size, size)

isEmptyFM fm = sizeFM fm == 0

lookupFM EmptyFM key = Nothing
lookupFM (Branch key elt _ fm_l fm_r) key_to_find
  = case compare key_to_find key of
	LT -> lookupFM fm_l key_to_find
	GT -> lookupFM fm_r key_to_find
	EQ -> Just elt

key `elemFM` fm
  = case (lookupFM fm key) of { Nothing -> False; Just elt -> True }

lookupWithDefaultFM fm deflt key
  = case (lookupFM fm key) of { Nothing -> deflt; Just elt -> elt }
\end{code}

%************************************************************************
%*									*
\subsection{Listifying @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm
keysFM fm   = foldFM (\ key elt rest -> key : rest)       [] fm
eltsFM fm   = foldFM (\ key elt rest -> elt : rest)       [] fm
\end{code}


%************************************************************************
%*									*
\subsection{The implementation of balancing}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection{Basic construction of a @FiniteMap@}
%*									*
%************************************************************************

@mkBranch@ simply gets the size component right.  This is the ONLY
(non-trivial) place the Branch object is built, so the ASSERTion
recursively checks consistency.  (The trivial use of Branch is in
@unitFM@.)

\begin{code}
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: (Ord key OUTPUTABLE_key) 		-- Used for the assertion checking only
	 => Int
	 -> key -> elt
	 -> FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

mkBranch which key elt fm_l fm_r
  = --ASSERT( left_ok && right_ok && balance_ok )
#if defined(DEBUG_FINITEMAPS)
    if not ( left_ok && right_ok && balance_ok ) then
	pprPanic ("mkBranch:"++show which) (vcat [ppr [left_ok, right_ok, balance_ok],
				       ppr key,
				       ppr fm_l,
				       ppr fm_r])
    else
#endif
    let
	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
--    if sizeFM result <= 8 then
	result
--    else
--	pprTrace ("mkBranch:"++(show which)) (ppr result) (
--	result
--	)
  where
    left_ok  = case fm_l of
		EmptyFM		         -> True
		Branch left_key _ _ _ _  -> let
						biggest_left_key = fst (findMax fm_l)
					    in
					    biggest_left_key < key
    right_ok = case fm_r of
		EmptyFM		         -> True
		Branch right_key _ _ _ _ -> let
						smallest_right_key = fst (findMin fm_r)
					    in
					    key < smallest_right_key
    balance_ok = True -- sigh
{- LATER:
    balance_ok
      = -- Both subtrees have one or no elements...
	(left_size + right_size <= 1)
-- NO	      || left_size == 0  -- ???
-- NO	      || right_size == 0 -- ???
    	-- ... or the number of elements in a subtree does not exceed
	-- sIZE_RATIO times the number of elements in the other subtree
      || (left_size  * sIZE_RATIO >= right_size &&
    	  right_size * sIZE_RATIO >= left_size)
-}

    left_size  = sizeFM fm_l
    right_size = sizeFM fm_r

#ifdef __GLASGOW_HASKELL__
    unbox :: Int -> Int#
    unbox (I# size) = size
#else
    unbox :: Int -> Int
    unbox x = x
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection{{\em Balanced} construction of a @FiniteMap@}
%*									*
%************************************************************************

@mkBalBranch@ rebalances, assuming that the subtrees aren't too far
out of whack.

\begin{code}
mkBalBranch :: (Ord key OUTPUTABLE_key)
	    => key -> elt
	    -> FiniteMap key elt -> FiniteMap key elt
	    -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l	-- Right tree too big
  = case fm_R of
	Branch _ _ _ fm_rl fm_rr
		| sizeFM fm_rl < 2 * sizeFM fm_rr -> single_L fm_L fm_R
		| otherwise	   	          -> double_L fm_L fm_R
	-- Other case impossible

  | size_l > sIZE_RATIO * size_r	-- Left tree too big
  = case fm_L of
	Branch _ _ _ fm_ll fm_lr
		| sizeFM fm_lr < 2 * sizeFM fm_ll -> single_R fm_L fm_R
		| otherwise		          -> double_R fm_L fm_R
	-- Other case impossible

  | otherwise				-- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM fm_L
    size_r   = sizeFM fm_R

    single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr)
	= mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr

    double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
	= mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
				 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)

    single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
	= mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)

    double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
	= mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
				 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
\end{code}


\begin{code}
mkVBalBranch :: (Ord key OUTPUTABLE_key)
	     => key -> elt
	     -> FiniteMap key elt -> FiniteMap key elt
	     -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--	   (a) all keys in l are < all keys in r
--	   (b) all keys in l are < key
--	   (c) all keys in r are > key

mkVBalBranch key elt EmptyFM fm_r = addToFM fm_r key elt
mkVBalBranch key elt fm_l EmptyFM = addToFM fm_l key elt

mkVBalBranch key elt fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
		     fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r
\end{code}

%************************************************************************
%*									*
\subsubsection{Gluing two trees together}
%*									*
%************************************************************************

@glueBal@ assumes its two arguments aren't too far out of whack, just
like @mkBalBranch@.  But: all keys in first arg are $<$ all keys in
second.

\begin{code}
glueBal :: (Ord key OUTPUTABLE_key)
	=> FiniteMap key elt -> FiniteMap key elt
	-> FiniteMap key elt

glueBal EmptyFM fm2 = fm2
glueBal fm1 EmptyFM = fm1
glueBal fm1 fm2
	-- The case analysis here (absent in Adams' program) is really to deal
	-- with the case where fm2 is a singleton. Then deleting the minimum means
	-- we pass an empty tree to mkBalBranch, which breaks its invariant.
  | sizeFM fm2 > sizeFM fm1
  = mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)

  | otherwise
  = mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2
  where
    (mid_key1, mid_elt1) = findMax fm1
    (mid_key2, mid_elt2) = findMin fm2
\end{code}

@glueVBal@ copes with arguments which can be of any size.
But: all keys in first arg are $<$ all keys in second.

\begin{code}
glueVBal :: (Ord key OUTPUTABLE_key)
	 => FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

glueVBal EmptyFM fm2 = fm2
glueVBal fm1 EmptyFM = fm1
glueVBal fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
	 fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

  | otherwise		-- We now need the same two cases as in glueBal above.
  = glueBal fm_l fm_r
  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r
\end{code}

%************************************************************************
%*									*
\subsection{Local utilities}
%*									*
%************************************************************************

\begin{code}
splitLT, splitGT :: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT EmptyFM split_key = emptyFM
splitLT (Branch key elt _ fm_l fm_r) split_key
  = case compare split_key key of
	LT -> splitLT fm_l split_key
	GT -> mkVBalBranch key elt fm_l (splitLT fm_r split_key)
	EQ -> fm_l

splitGT EmptyFM split_key = emptyFM
splitGT (Branch key elt _ fm_l fm_r) split_key
  = case compare split_key key of
	GT -> splitGT fm_r split_key
	LT -> mkVBalBranch key elt (splitGT fm_l split_key) fm_r
	EQ -> fm_r

findMin :: FiniteMap key elt -> (key,elt)
findMin (Branch key elt _ EmptyFM _) = (key,elt)
findMin (Branch key elt _ fm_l    _) = findMin fm_l

deleteMin :: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt
deleteMin (Branch key elt _ EmptyFM fm_r) = fm_r
deleteMin (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt (deleteMin fm_l) fm_r

findMax :: FiniteMap key elt -> (key,elt)
findMax (Branch key elt _ _ EmptyFM) = (key,elt)
findMax (Branch key elt _ _    fm_r) = findMax fm_r

deleteMax :: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt
deleteMax (Branch key elt _ fm_l EmptyFM) = fm_l
deleteMax (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt fm_l (deleteMax fm_r)
\end{code}

%************************************************************************
%*									*
\subsection{Output-ery}
%*									*
%************************************************************************

\begin{code}
#if defined(DEBUG_FINITEMAPS)

instance (Outputable key) => Outputable (FiniteMap key elt) where
    ppr fm = pprX fm

pprX EmptyFM = char '!'
pprX (Branch key elt sz fm_l fm_r)
 = parens (hcat [pprX fm_l, space,
		      ppr key, space, int (IF_GHC(I# sz, sz)), space,
		      pprX fm_r])
#else
-- and when not debugging the package itself...
instance (Outputable key, Outputable elt) => Outputable (FiniteMap key elt) where
    ppr fm = ppr (fmToList fm)
#endif

#if 0
instance (Eq key, Eq elt) => Eq (FiniteMap key elt) where
  fm_1 == fm_2 = (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 == fmToList fm_2)

{- NO: not clear what The Right Thing to do is:
instance (Ord key, Ord elt) => Ord (FiniteMap key elt) where
  fm_1 <= fm_2 = (sizeFM   fm_1 <= sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 <= fmToList fm_2)
-}
#endif
\end{code}

%************************************************************************
%*									*
\subsection{Efficiency pragmas for GHC}
%*									*
%************************************************************************

When the FiniteMap module is used in GHC, we specialise it for
\tr{Uniques}, for dastardly efficiency reasons.

\begin{code}
#if 0

#if __GLASGOW_HASKELL__

{-# SPECIALIZE addListToFM
		:: FiniteMap (FastString, FAST_STRING) elt -> [((FAST_STRING, FAST_STRING),elt)] -> FiniteMap (FAST_STRING, FAST_STRING) elt
		 , FiniteMap RdrName elt -> [(RdrName,elt)] -> FiniteMap RdrName elt
    IF_NCG(COMMA   FiniteMap Reg elt -> [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addListToFM_C
		:: (elt -> elt -> elt) -> FiniteMap TyCon elt -> [(TyCon,elt)] -> FiniteMap TyCon elt
		 , (elt -> elt -> elt) -> FiniteMap FastString elt -> [(FAST_STRING,elt)] -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addToFM
		:: FiniteMap CLabel elt -> CLabel -> elt  -> FiniteMap CLabel elt
		 , FiniteMap FastString elt -> FAST_STRING -> elt  -> FiniteMap FAST_STRING elt
		 , FiniteMap (FastString, FAST_STRING) elt -> (FAST_STRING, FAST_STRING) -> elt  -> FiniteMap (FAST_STRING, FAST_STRING) elt
		 , FiniteMap RdrName elt -> RdrName -> elt  -> FiniteMap RdrName elt
    IF_NCG(COMMA   FiniteMap Reg elt -> Reg -> elt  -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addToFM_C
		:: (elt -> elt -> elt) -> FiniteMap (RdrName, RdrName) elt -> (RdrName, RdrName) -> elt -> FiniteMap (RdrName, RdrName) elt
		 , (elt -> elt -> elt) -> FiniteMap FastString elt -> FAST_STRING -> elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> Reg -> elt -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE bagToFM
		:: Bag (FastString,elt) -> FiniteMap FAST_STRING elt
    #-}
{-# SPECIALIZE delListFromFM
		:: FiniteMap RdrName elt -> [RdrName]   -> FiniteMap RdrName elt
		 , FiniteMap FastString elt -> [FAST_STRING]   -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   FiniteMap Reg elt -> [Reg]   -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE listToFM
		:: [([Char],elt)] -> FiniteMap [Char] elt
		 , [(FastString,elt)] -> FiniteMap FAST_STRING elt
		 , [((FastString,FAST_STRING),elt)] -> FiniteMap (FAST_STRING, FAST_STRING) elt
    IF_NCG(COMMA   [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE lookupFM
		:: FiniteMap CLabel elt -> CLabel -> Maybe elt
		 , FiniteMap [Char] elt -> [Char] -> Maybe elt
		 , FiniteMap FastString elt -> FAST_STRING -> Maybe elt
		 , FiniteMap (FastString,FAST_STRING) elt -> (FAST_STRING,FAST_STRING) -> Maybe elt
		 , FiniteMap RdrName elt -> RdrName -> Maybe elt
		 , FiniteMap (RdrName,RdrName) elt -> (RdrName,RdrName) -> Maybe elt
    IF_NCG(COMMA   FiniteMap Reg elt -> Reg -> Maybe elt)
    #-}
{-# SPECIALIZE lookupWithDefaultFM
		:: FiniteMap FastString elt -> elt -> FAST_STRING -> elt
    IF_NCG(COMMA   FiniteMap Reg elt -> elt -> Reg -> elt)
    #-}
{-# SPECIALIZE plusFM
		:: FiniteMap RdrName elt -> FiniteMap RdrName elt -> FiniteMap RdrName elt
		 , FiniteMap FastString elt -> FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   FiniteMap Reg elt -> FiniteMap Reg elt -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE plusFM_C
		:: (elt -> elt -> elt) -> FiniteMap FastString elt -> FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> FiniteMap Reg elt -> FiniteMap Reg elt)
    #-}

#endif /* compiling with ghc and have specialiser */

#endif /* 0 */
\end{code}
