-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteMap
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A finite map implementation, derived from the paper:
-- 	   /Efficient sets: a balancing act/, S. Adams,
-- 	   Journal of functional programming 3(4) Oct 1993, pp553-562
--
-----------------------------------------------------------------------------

-- ToDo: clean up, remove the COMPILING_GHC stuff.

-- The code is SPECIALIZEd to various highly-desirable types (e.g., Id)
-- near the end (only \tr{#ifdef COMPILING_GHC}).

#ifdef COMPILING_GHC
#include "HsVersions.h"
#define IF_NOT_GHC(a) {--}
#else
#define ASSERT(e) {--}
#define IF_NOT_GHC(a) a
#define COMMA ,
#define _tagCmp compare
#define _LT LT
#define _GT GT
#define _EQ EQ
#endif

#if defined(COMPILING_GHC) && defined(DEBUG_FINITEMAPS)/* NB NB NB */
#define OUTPUTABLE_key , Outputable key
#else
#define OUTPUTABLE_key {--}
#endif

module Data.FiniteMap (
	-- * The @FiniteMap@ type
	FiniteMap,		-- abstract type

	-- * Construction
	emptyFM, unitFM, listToFM,

	-- * Lookup operations
	lookupFM, lookupWithDefaultFM,
	elemFM,

	-- * Adding elements
	addToFM,
	addToFM_C,
	addListToFM,
	addListToFM_C,

	-- * Deleting elements
	IF_NOT_GHC(delFromFM COMMA)
	delListFromFM,

	-- * Combination
	plusFM,
	plusFM_C,

	-- * Extracting information
	fmToList, keysFM, eltsFM,
	sizeFM, isEmptyFM,

	-- * Other operations
	minusFM,
	foldFM,
	IF_NOT_GHC(intersectFM COMMA)
	IF_NOT_GHC(intersectFM_C COMMA)
	IF_NOT_GHC(mapFM COMMA filterFM COMMA)

	foldFM_GE, fmToList_GE, keysFM_GE, eltsFM_GE,
	foldFM_LE, fmToList_LE, keysFM_LE, eltsFM_LE,

        minFM, maxFM,

#ifdef COMPILING_GHC
	, bagToFM
#endif
    ) where

import Data.Maybe ( isJust )
#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

#ifdef __HADDOCK__
import Prelude
#endif

#ifdef COMPILING_GHC
IMP_Ubiq(){-uitous-}
# ifdef DEBUG
import Pretty
# endif
import Bag	( foldBag )

# if ! OMIT_NATIVE_CODEGEN
#  define IF_NCG(a) a
# else
#  define IF_NCG(a) {--}
# endif
#endif

-- SIGH: but we use unboxed "sizes"...
#if __GLASGOW_HASKELL__
#define IF_GHC(a,b) a
#else /* not GHC */
#define IF_GHC(a,b) b
#endif /* not GHC */


-- ---------------------------------------------------------------------------
-- The signature of the module

-- | An empty 'FiniteMap'.
emptyFM		:: FiniteMap key elt

-- | A 'FiniteMap' containing a single mapping
unitFM		:: key -> elt -> FiniteMap key elt

-- | Makes a 'FiniteMap' from a list of @(key,value)@ pairs. In the
-- case of duplicates, the last is taken
listToFM	:: (Ord key OUTPUTABLE_key) => [(key,elt)] -> FiniteMap key elt

#ifdef COMPILING_GHC
bagToFM		:: (Ord key OUTPUTABLE_key) => Bag (key,elt) -> FiniteMap key elt
			-- In the case of duplicates, who knows which is taken
#endif

--	ADDING AND DELETING

-- | Adds an element to a 'FiniteMap'.  Any previous mapping with the same
-- key is overwritten.
addToFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> elt  -> FiniteMap key elt

-- | Adds a list of elements to a 'FiniteMap', in the order given in
-- the list.  Overwrites previous mappings.
addListToFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

		   -- Combines with previous binding
		   -- In the combining function, the first argument is the "old" element,
		   -- while the second is the "new" one.

-- | Adds an element to a 'FiniteMap'.  If there is already an element
-- with the same key, then the specified combination function is used
-- to calculate the new value. The already present element is passed as
-- the first argument and the new element to add as second.
addToFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> key -> elt
			   -> FiniteMap key elt

-- | A list version of 'addToFM_C'.  The elements are added in the
-- order given in the list.
addListToFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> [(key,elt)]
			   -> FiniteMap key elt

-- | Deletes an element from a 'FiniteMap'.  If there is no element with
-- the specified key, then the original 'FiniteMap' is returned.
delFromFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key   -> FiniteMap key elt

-- | List version of 'delFromFM'.
delListFromFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> [key] -> FiniteMap key elt

-- | Combine two 'FiniteMap's.  Mappings in the second argument shadow
-- those in the first.
plusFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt
			   -> FiniteMap key elt

-- | Combine two 'FiniteMap's.  The specified combination function is
-- used to calculate the new value when there are two elements with
-- the same key.
plusFM_C	:: (Ord key OUTPUTABLE_key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

-- | @(minusFM a1 a2)@ deletes from @a1@ any mappings which are bound in @a2@
minusFM		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

-- | @(intersectFM a1 a2)@ returns a new 'FiniteMap' containing
-- mappings from @a1@ for which @a2@ also has a mapping with the same
-- key.
intersectFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

-- | Returns the interesction of two mappings, using the specified
-- combination function to combine values.
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

-- | Returns 'True' if the specified @key@ has a mapping in this
-- 'FiniteMap', or 'False' otherwise.
elemFM		:: (Ord key OUTPUTABLE_key) => key -> FiniteMap key elt -> Bool

-- | Looks up a key in a 'FiniteMap', returning @'Just' v@ if the key
-- was found with value @v@, or 'Nothing' otherwise.
lookupFM	:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> Maybe elt

-- | Looks up a key in a 'FiniteMap', returning @elt@ if the specified
-- @key@ was not found.
lookupWithDefaultFM
		:: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> elt -> key -> elt
		-- lookupWithDefaultFM supplies a "default" elt
		-- to return for an unmapped key

--	LISTIFYING

-- | Convert a 'FiniteMap' to a @[(key, elt)]@ sorted by 'Ord' key
--
fmToList	:: FiniteMap key elt -> [(key,elt)]

-- | Extract the keys from a 'FiniteMap', in the order of the keys, so
--
-- > keysFM == map fst . fmToList
--
keysFM		:: FiniteMap key elt -> [key]

-- | Extract the elements from a 'FiniteMap', in the order of the keys, so
--
-- > eltsFM == map snd . fmToList
--
eltsFM		:: FiniteMap key elt -> [elt]

-- ---------------------------------------------------------------------------
-- The @FiniteMap@ data type, and building of same

-- Invariants about @FiniteMap@:
--
-- *  all keys in a FiniteMap are distinct
--
-- * all keys in left  subtree are $<$ key in Branch and
--   all keys in right subtree are $>$ key in Branch
-- 
-- * size field of a Branch gives number of Branch nodes in the tree
-- 
-- * size of left subtree is differs from size of right subtree by a
--   factor of at most \tr{sIZE_RATIO}

-- | A mapping from @key@s to @elt@s.
data FiniteMap key elt
  = EmptyFM
  | Branch key elt	    	-- Key and elt stored here
    IF_GHC(Int#,Int{-STRICT-})	-- Size >= 1
    (FiniteMap key elt)	    	-- Children
    (FiniteMap key elt)


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

#ifdef COMPILING_GHC
bagToFM = foldBag plusFM (\ (k,v) -> unitFM k v) emptyFM
#endif


-- ---------------------------------------------------------------------------
-- Adding to and deleting from @FiniteMaps@

addToFM fm key elt = addToFM_C (\ old new -> new) fm key elt

addToFM_C combiner EmptyFM key elt = unitFM key elt
addToFM_C combiner (Branch key elt size fm_l fm_r) new_key new_elt
#ifdef __GLASGOW_HASKELL__
  = case _tagCmp new_key key of
	_LT -> mkBalBranch key elt (addToFM_C combiner fm_l new_key new_elt) fm_r
	_GT -> mkBalBranch key elt fm_l (addToFM_C combiner fm_r new_key new_elt)
	_EQ -> Branch new_key (combiner elt new_elt) size fm_l fm_r
#else
  | new_key < key = mkBalBranch key elt (addToFM_C combiner fm_l new_key new_elt) fm_r
  | new_key > key = mkBalBranch key elt fm_l (addToFM_C combiner fm_r new_key new_elt)
  | otherwise	  = Branch new_key (combiner elt new_elt) size fm_l fm_r
#endif

addListToFM fm key_elt_pairs = addListToFM_C (\ old new -> new) fm key_elt_pairs

addListToFM_C combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs	-- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C combiner fmap key elt


delFromFM EmptyFM del_key = emptyFM
delFromFM (Branch key elt size fm_l fm_r) del_key
#if __GLASGOW_HASKELL__
  = case _tagCmp del_key key of
	_GT -> mkBalBranch key elt fm_l (delFromFM fm_r del_key)
	_LT -> mkBalBranch key elt (delFromFM fm_l del_key) fm_r
	_EQ -> glueBal fm_l fm_r
#else
  | del_key > key
  = mkBalBranch key elt fm_l (delFromFM fm_r del_key)

  | del_key < key
  = mkBalBranch key elt (delFromFM fm_l del_key) fm_r

  | key == del_key
  = glueBal fm_l fm_r
#endif

delListFromFM fm keys = foldl delFromFM fm keys

-- ---------------------------------------------------------------------------
-- Combining @FiniteMaps@

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

  | isJust maybe_elt1	-- split_elt *is* in intersection
  = mkVBalBranch split_key (combiner elt1 elt2) (intersectFM_C combiner lts left)
						(intersectFM_C combiner gts right)

  | otherwise			-- split_elt is *not* in intersection
  = glueVBal (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)

  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

    maybe_elt1 = lookupFM fm1 split_key
    Just elt1  = maybe_elt1


-- ---------------------------------------------------------------------------
-- Mapping, folding, and filtering with @FiniteMaps@

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


-- ---------------------------------------------------------------------------
-- Interrogating @FiniteMaps@

--{-# INLINE sizeFM #-}
sizeFM EmptyFM		     = 0
sizeFM (Branch _ _ size _ _) = IF_GHC(I# size, size)

isEmptyFM fm = sizeFM fm == 0

lookupFM EmptyFM key = Nothing
lookupFM (Branch key elt _ fm_l fm_r) key_to_find
#if __GLASGOW_HASKELL__
  = case _tagCmp key_to_find key of
	_LT -> lookupFM fm_l key_to_find
	_GT -> lookupFM fm_r key_to_find
	_EQ -> Just elt
#else
  | key_to_find < key = lookupFM fm_l key_to_find
  | key_to_find > key = lookupFM fm_r key_to_find
  | otherwise	  = Just elt
#endif

key `elemFM` fm
  = case (lookupFM fm key) of { Nothing -> False; Just elt -> True }

lookupWithDefaultFM fm deflt key
  = case (lookupFM fm key) of { Nothing -> deflt; Just elt -> elt }


-- ---------------------------------------------------------------------------
-- Listifying @FiniteMaps@

fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm
keysFM fm   = foldFM (\ key elt rest -> key : rest)       [] fm
eltsFM fm   = foldFM (\ key elt rest -> elt : rest)       [] fm


-- ---------------------------------------------------------------------------
-- Bulk operations on all keys >= or <=	a certain threshold

-- | Fold through all elements greater than or equal to the supplied key,
-- in increasing order.
foldFM_GE       :: Ord key => (key -> elt -> a -> a) -> a -> key ->
   FiniteMap key elt -> a

foldFM_GE k z fr EmptyFM = z
foldFM_GE k z fr (Branch key elt _ fm_l fm_r)
  | key >= fr = foldFM_GE k (k key elt (foldFM_GE k z fr fm_r)) fr fm_l
  | otherwise = foldFM_GE k z fr fm_r

-- | List elements greater than or equal to the supplied key, in increasing
-- order
fmToList_GE      :: Ord key => FiniteMap key elt -> key ->  [(key,elt)]
fmToList_GE fm fr = foldFM_GE (\ key elt rest -> (key,elt) : rest) [] fr fm

-- | List keys greater than or equal to the supplied key, in increasing order
keysFM_GE       :: Ord key => FiniteMap key elt -> key -> [key]
keysFM_GE fm fr  = foldFM_GE (\ key elt rest -> key : rest)       [] fr fm

-- | List elements corresponding to keys greater than or equal to the supplied
-- key, in increasing order of key.
eltsFM_GE       :: Ord key => FiniteMap key elt -> key -> [elt]
eltsFM_GE fm fr  = foldFM_GE (\ key elt rest -> elt : rest)       [] fr fm

-- | Fold through all elements less than or equal to the supplied key,
-- in decreasing order.
foldFM_LE       :: Ord key => (key -> elt -> a -> a) -> a -> key ->
   FiniteMap key elt -> a
foldFM_LE k z fr EmptyFM = z
foldFM_LE k z fr (Branch key elt _ fm_l fm_r)
  | key <= fr = foldFM_LE k (k key elt (foldFM_LE k z fr fm_l)) fr fm_r
  | otherwise = foldFM_LE k z fr fm_l

-- | List elements greater than or equal to the supplied key, in decreasing
-- order
fmToList_LE      :: Ord key => FiniteMap key elt -> key ->  [(key,elt)]
fmToList_LE fm fr = foldFM_LE (\ key elt rest -> (key,elt) : rest) [] fr fm

-- | List keys greater than or equal to the supplied key, in decreasing order
keysFM_LE       :: Ord key => FiniteMap key elt -> key -> [key]
keysFM_LE fm fr  = foldFM_LE (\ key elt rest -> key : rest)       [] fr fm

-- | List elements corresponding to keys greater than or equal to the supplied
-- key, in decreasing order of key.
eltsFM_LE       :: Ord key => FiniteMap key elt -> key -> [elt]
eltsFM_LE fm fr  = foldFM_LE (\ key elt rest -> elt : rest)       [] fr fm

-- ---------------------------------------------------------------------------
-- Getting minimum and maximum key out.
-- ---------------------------------------------------------------------------

-- | Extract minimum key, or Nothing if the map is empty.
minFM :: Ord key => FiniteMap key elt -> Maybe key
minFM EmptyFM = Nothing
minFM (Branch key _ _ fm_l _) =
   case minFM fm_l of
      Nothing -> Just key
      Just key1 -> Just key1

-- | Extract maximum key, or Nothing if the map is empty.
maxFM :: Ord key => FiniteMap key elt -> Maybe key
maxFM EmptyFM = Nothing
maxFM (Branch key _ _ _ fm_r) =
   case maxFM fm_r of
      Nothing -> Just key
      Just key1 -> Just key1


-- ---------------------------------------------------------------------------
-- The implementation of balancing

-- Basic construction of a @FiniteMap@:

-- @mkBranch@ simply gets the size component right.  This is the ONLY
-- (non-trivial) place the Branch object is built, so the ASSERTion
-- recursively checks consistency.  (The trivial use of Branch is in
-- @unitFM@.)

sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: (Ord key OUTPUTABLE_key) 		-- Used for the assertion checking only
	 => Int
	 -> key -> elt
	 -> FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

mkBranch which key elt fm_l fm_r
  = --ASSERT( left_ok && right_ok && balance_ok )
#if defined(COMPILING_GHC) && defined(DEBUG_FINITEMAPS)
    if not ( left_ok && right_ok && balance_ok ) then
	pprPanic ("mkBranch:"++show which) (ppAboves [ppr PprDebug [left_ok, right_ok, balance_ok],
				       ppr PprDebug key,
				       ppr PprDebug fm_l,
				       ppr PprDebug fm_r])
    else
#endif
    let
	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
--    if sizeFM result <= 8 then
	result
--    else
--	pprTrace ("mkBranch:"++(show which)) (ppr PprDebug result) (
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

#if __GLASGOW_HASKELL__
    unbox :: Int -> Int#
    unbox (I# size) = size
#else
    unbox :: Int -> Int
    unbox x = x
#endif


-- ---------------------------------------------------------------------------
-- {\em Balanced} construction of a @FiniteMap@

-- @mkBalBranch@ rebalances, assuming that the subtrees aren't too far
-- out of whack.

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

-- ---------------------------------------------------------------------------
-- Gluing two trees together

-- @glueBal@ assumes its two arguments aren't too far out of whack, just
-- like @mkBalBranch@.  But: all keys in first arg are $<$ all keys in
-- second.

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

-- @glueVBal@ copes with arguments which can be of any size.
-- But: all keys in first arg are $<$ all keys in second.

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


-- ---------------------------------------------------------------------------
-- Local utilities

splitLT, splitGT :: (Ord key OUTPUTABLE_key) => FiniteMap key elt -> key -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT EmptyFM split_key = emptyFM
splitLT (Branch key elt _ fm_l fm_r) split_key
#if __GLASGOW_HASKELL__
  = case _tagCmp split_key key of
	_LT -> splitLT fm_l split_key
	_GT -> mkVBalBranch key elt fm_l (splitLT fm_r split_key)
	_EQ -> fm_l
#else
  | split_key < key = splitLT fm_l split_key
  | split_key > key = mkVBalBranch key elt fm_l (splitLT fm_r split_key)
  | otherwise	    = fm_l
#endif

splitGT EmptyFM split_key = emptyFM
splitGT (Branch key elt _ fm_l fm_r) split_key
#if __GLASGOW_HASKELL__
  = case _tagCmp split_key key of
	_GT -> splitGT fm_r split_key
	_LT -> mkVBalBranch key elt (splitGT fm_l split_key) fm_r
	_EQ -> fm_r
#else
  | split_key > key = splitGT fm_r split_key
  | split_key < key = mkVBalBranch key elt (splitGT fm_l split_key) fm_r
  | otherwise	    = fm_r
#endif

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


-- ---------------------------------------------------------------------------
-- Output-ery

#if defined(COMPILING_GHC) && defined(DEBUG_FINITEMAPS)

instance (Outputable key) => Outputable (FiniteMap key elt) where
    ppr sty fm = pprX sty fm

pprX sty EmptyFM = ppChar '!'
pprX sty (Branch key elt sz fm_l fm_r)
 = ppBesides [ppLparen, pprX sty fm_l, ppSP,
	      ppr sty key, ppSP, ppInt (IF_GHC(I# sz, sz)), ppSP,
	      pprX sty fm_r, ppRparen]
#endif

#ifndef COMPILING_GHC
instance (Eq key, Eq elt) => Eq (FiniteMap key elt) where
  fm_1 == fm_2 = (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 == fmToList fm_2)

{- NO: not clear what The Right Thing to do is:
instance (Ord key, Ord elt) => Ord (FiniteMap key elt) where
  fm_1 <= fm_2 = (sizeFM   fm_1 <= sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 <= fmToList fm_2)
-}
#endif

-- ---------------------------------------------------------------------------
-- Efficiency pragmas for GHC

-- When the FiniteMap module is used in GHC, we specialise it for
-- \tr{Uniques}, for dastardly efficiency reasons.

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ && !defined(REALLY_HASKELL_1_3)

{-# SPECIALIZE addListToFM
		:: FiniteMap (FAST_STRING, FAST_STRING) elt -> [((FAST_STRING, FAST_STRING),elt)] -> FiniteMap (FAST_STRING, FAST_STRING) elt
		 , FiniteMap RdrName elt -> [(RdrName,elt)] -> FiniteMap RdrName elt
    IF_NCG(COMMA   FiniteMap Reg elt -> [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addListToFM_C
		:: (elt -> elt -> elt) -> FiniteMap TyCon elt -> [(TyCon,elt)] -> FiniteMap TyCon elt
		 , (elt -> elt -> elt) -> FiniteMap FAST_STRING elt -> [(FAST_STRING,elt)] -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addToFM
		:: FiniteMap CLabel elt -> CLabel -> elt  -> FiniteMap CLabel elt
		 , FiniteMap FAST_STRING elt -> FAST_STRING -> elt  -> FiniteMap FAST_STRING elt
		 , FiniteMap (FAST_STRING, FAST_STRING) elt -> (FAST_STRING, FAST_STRING) -> elt  -> FiniteMap (FAST_STRING, FAST_STRING) elt
		 , FiniteMap RdrName elt -> RdrName -> elt  -> FiniteMap RdrName elt
		 , FiniteMap OrigName elt -> OrigName -> elt  -> FiniteMap OrigName elt
    IF_NCG(COMMA   FiniteMap Reg elt -> Reg -> elt  -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE addToFM_C
		:: (elt -> elt -> elt) -> FiniteMap (RdrName, RdrName) elt -> (RdrName, RdrName) -> elt -> FiniteMap (RdrName, RdrName) elt
		 , (elt -> elt -> elt) -> FiniteMap (OrigName, OrigName) elt -> (OrigName, OrigName) -> elt -> FiniteMap (OrigName, OrigName) elt
		 , (elt -> elt -> elt) -> FiniteMap FAST_STRING elt -> FAST_STRING -> elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> Reg -> elt -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE bagToFM
		:: Bag (FAST_STRING,elt) -> FiniteMap FAST_STRING elt
    #-}
{-# SPECIALIZE delListFromFM
		:: FiniteMap RdrName elt -> [RdrName]   -> FiniteMap RdrName elt
		 , FiniteMap OrigName elt -> [OrigName]   -> FiniteMap OrigName elt
		 , FiniteMap FAST_STRING elt -> [FAST_STRING]   -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   FiniteMap Reg elt -> [Reg]   -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE listToFM
		:: [([Char],elt)] -> FiniteMap [Char] elt
		 , [(FAST_STRING,elt)] -> FiniteMap FAST_STRING elt
		 , [((FAST_STRING,FAST_STRING),elt)] -> FiniteMap (FAST_STRING, FAST_STRING) elt
		 , [(OrigName,elt)] -> FiniteMap OrigName elt
    IF_NCG(COMMA   [(Reg COMMA elt)] -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE lookupFM
		:: FiniteMap CLabel elt -> CLabel -> Maybe elt
		 , FiniteMap [Char] elt -> [Char] -> Maybe elt
		 , FiniteMap FAST_STRING elt -> FAST_STRING -> Maybe elt
		 , FiniteMap (FAST_STRING,FAST_STRING) elt -> (FAST_STRING,FAST_STRING) -> Maybe elt
		 , FiniteMap OrigName elt -> OrigName -> Maybe elt
		 , FiniteMap (OrigName,OrigName) elt -> (OrigName,OrigName) -> Maybe elt
		 , FiniteMap RdrName elt -> RdrName -> Maybe elt
		 , FiniteMap (RdrName,RdrName) elt -> (RdrName,RdrName) -> Maybe elt
    IF_NCG(COMMA   FiniteMap Reg elt -> Reg -> Maybe elt)
    #-}
{-# SPECIALIZE lookupWithDefaultFM
		:: FiniteMap FAST_STRING elt -> elt -> FAST_STRING -> elt
    IF_NCG(COMMA   FiniteMap Reg elt -> elt -> Reg -> elt)
    #-}
{-# SPECIALIZE plusFM
		:: FiniteMap RdrName elt -> FiniteMap RdrName elt -> FiniteMap RdrName elt
		 , FiniteMap OrigName elt -> FiniteMap OrigName elt -> FiniteMap OrigName elt
		 , FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   FiniteMap Reg elt -> FiniteMap Reg elt -> FiniteMap Reg elt)
    #-}
{-# SPECIALIZE plusFM_C
		:: (elt -> elt -> elt) -> FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt -> FiniteMap FAST_STRING elt
    IF_NCG(COMMA   (elt -> elt -> elt) -> FiniteMap Reg elt -> FiniteMap Reg elt -> FiniteMap Reg elt)
    #-}

#endif /* compiling for GHC */
