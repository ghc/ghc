%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

``Finite maps'' are the heart of the compiler's lookup-tables/environments
and its implementation of sets. Important stuff!

The implementation uses @Data.Map@ from the containers package, which
is both maintained and faster than the past implementation (see commit log).

The orinigal interface is being kept around. It maps directly to Data.Map,
only ``Data.Map.union'' is left-biased and ``plusFM'' right-biased and
``addToFM\_C'' and ``Data.Map.insertWith'' differ in the order of
arguments of combining function.

\begin{code}
module FiniteMap (
        -- * Mappings keyed from arbitrary types
        FiniteMap,   -- abstract data type

        -- ** Manipulating those mappings
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

        fmToList, keysFM, eltsFM,

        bagToFM
    ) where

import Bag ( Bag, foldrBag )
import Outputable

import qualified Data.Map as M

\end{code}


%************************************************************************
%*                                                                      *
\subsection{The signature of the module}
%*                                                                      *
%************************************************************************

\begin{code}
-- BUILDING
emptyFM     :: FiniteMap key elt
unitFM      :: key -> elt -> FiniteMap key elt
-- | In the case of duplicates keys, the last item is taken
listToFM    :: (Ord key) => [(key,elt)] -> FiniteMap key elt
-- | In the case of duplicate keys, who knows which item is taken
bagToFM     :: (Ord key) => Bag (key,elt) -> FiniteMap key elt

-- ADDING AND DELETING

-- | Throws away any previous binding
addToFM     :: (Ord key)
            => FiniteMap key elt -> key -> elt -> FiniteMap key elt
-- | Throws away any previous binding, items are added left-to-right
addListToFM :: (Ord key)
            => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

-- | Combines added item with previous item, if any --
-- if the key is present, ``addToFM_C f`` inserts
-- ``(key, f old_value new_value)''
addToFM_C       :: (Ord key) => (elt -> elt -> elt)
                           -> FiniteMap key elt -> key -> elt
                           -> FiniteMap key elt
-- | Combines added item with previous item, if any, items are added left-to-right
addListToFM_C   :: (Ord key) => (elt -> elt -> elt)
                           -> FiniteMap key elt -> [(key,elt)]
                           -> FiniteMap key elt

-- | Deletion doesn't complain if you try to delete something which isn't there
delFromFM       :: (Ord key)
                => FiniteMap key elt -> key   -> FiniteMap key elt
-- | Deletion doesn't complain if you try to delete something which isn't there
delListFromFM   :: (Ord key)
                => FiniteMap key elt -> [key] -> FiniteMap key elt

-- COMBINING

-- | Bindings in right argument shadow those in the left
plusFM          :: (Ord key)
                => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

-- | Combines bindings for the same thing with the given function, 
-- bindings in right argument shadow those in the left
plusFM_C        :: (Ord key)
                => (elt -> elt -> elt)
                -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

-- | Deletes from the left argument any bindings in the right argument
minusFM         :: (Ord key)
                => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

intersectFM     :: (Ord key)
                => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
-- | Combines bindings for the same thing in the two maps with the given function
intersectFM_C   :: (Ord key)
                => (elt1 -> elt2 -> elt3)
                -> FiniteMap key elt1 -> FiniteMap key elt2
                -> FiniteMap key elt3

-- MAPPING, FOLDING, FILTERING
foldFM          :: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
mapFM           :: (key -> elt1 -> elt2)
                -> FiniteMap key elt1 -> FiniteMap key elt2
filterFM        :: (Ord key)
                => (key -> elt -> Bool)
                -> FiniteMap key elt -> FiniteMap key elt

-- INTERROGATING
sizeFM                  :: FiniteMap key elt -> Int
isEmptyFM               :: FiniteMap key elt -> Bool

elemFM                  :: (Ord key)
                        => key -> FiniteMap key elt -> Bool
lookupFM                :: (Ord key)
                        => FiniteMap key elt -> key -> Maybe elt
-- | Supplies a "default" element in return for an unmapped key
lookupWithDefaultFM     :: (Ord key)
                        => FiniteMap key elt -> elt -> key -> elt

-- LISTIFYING
fmToList        :: FiniteMap key elt -> [(key,elt)]
keysFM          :: FiniteMap key elt -> [key]
eltsFM          :: FiniteMap key elt -> [elt]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Implementation using ``Data.Map''}
%*                                                                      *
%************************************************************************

\begin{code}
newtype FiniteMap key elt = FM (M.Map key elt)

emptyFM = FM M.empty
unitFM k v = FM (M.singleton k v)
listToFM l = FM (M.fromList l)

addToFM (FM m) k v = FM (M.insert k v m)
-- Arguments of combining function of M.insertWith and addToFM_C are flipped.
addToFM_C f (FM m) k v = FM (M.insertWith (flip f) k v m)
addListToFM = foldl (\m (k, v) -> addToFM m k v)
addListToFM_C f = foldl (\m (k, v) -> addToFM_C f m k v)
delFromFM (FM m) k = FM (M.delete k m)
delListFromFM = foldl delFromFM

-- M.union is left-biased, plusFM should be right-biased.
plusFM (FM x) (FM y) = FM (M.union y x)
plusFM_C f (FM x) (FM y) = FM (M.unionWith f x y)
minusFM (FM x) (FM y) = FM (M.difference x y)
#if MIN_VERSION_containers(0,4,0)
foldFM k z (FM m) = M.foldrWithKey k z m
#else
foldFM k z (FM m) = M.foldWithKey k z m
#endif

intersectFM (FM x) (FM y) = FM (M.intersection x y)
intersectFM_C f (FM x) (FM y) = FM (M.intersectionWith f x y)
mapFM f (FM m) = FM (M.mapWithKey f m)
filterFM p (FM m) = FM (M.filterWithKey p m)

sizeFM (FM m) = M.size m
isEmptyFM (FM m) = M.null m
elemFM k (FM m) = M.member k m
lookupFM (FM m) k = M.lookup k m
lookupWithDefaultFM (FM m) v k = M.findWithDefault v k m

fmToList (FM m) = M.toList m
keysFM (FM m) = M.keys m
eltsFM (FM m) = M.elems m

bagToFM = foldrBag (\(k,v) m -> addToFM m k v) emptyFM

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Output-ery}
%*                                                                      *
%************************************************************************

\begin{code}
instance (Outputable key, Outputable elt) => Outputable (FiniteMap key elt) where
    ppr fm = ppr (fmToList fm)
\end{code}
