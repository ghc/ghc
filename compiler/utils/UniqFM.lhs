%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

UniqFM: Specialised finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

(A similar thing to @UniqSet@, as opposed to @Set@.)

The interface is based on @FiniteMap@s, but the implementation uses
@Data.IntMap@, which is both maintained and faster than the past
implementation (see commit log).

The @UniqFM@ interface maps directly to Data.IntMap, only
``Data.IntMap.union'' is left-biased and ``plusUFM'' right-biased
and ``addToUFM\_C'' and ``Data.IntMap.insertWith'' differ in the order
of arguments of combining function.

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS -Wall #-}
module UniqFM (
        -- * Unique-keyed mappings
        UniqFM,       -- abstract type

        -- ** Manipulating those mappings
        emptyUFM,
        unitUFM,
        unitDirectlyUFM,
        listToUFM,
        listToUFM_Directly,
        listToUFM_C,
        addToUFM,addToUFM_C,addToUFM_Acc,
        addListToUFM,addListToUFM_C,
        addToUFM_Directly,
        addListToUFM_Directly,
        adjustUFM, alterUFM,
        adjustUFM_Directly,
        delFromUFM,
        delFromUFM_Directly,
        delListFromUFM,
        plusUFM,
        plusUFM_C,
        minusUFM,
        intersectUFM,
        intersectUFM_C,
        foldUFM, foldUFM_Directly,
        mapUFM, mapUFM_Directly,
        mapMaybeUFM,
        elemUFM, elemUFM_Directly,
        filterUFM, filterUFM_Directly, partitionUFM,
        sizeUFM,
        isNullUFM,
        lookupUFM, lookupUFM_Directly,
        lookupWithDefaultUFM, lookupWithDefaultUFM_Directly,
        eltsUFM, keysUFM, splitUFM,
        ufmToList,
        joinUFM
    ) where

import Unique           ( Uniquable(..), Unique, getKey )
import Outputable

import Compiler.Hoopl   hiding (Unique)

import Data.Function (on)
import qualified Data.IntMap as M
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Typeable
import Data.Data
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The signature of the module}
%*                                                                      *
%************************************************************************

\begin{code}
emptyUFM        :: UniqFM elt
isNullUFM       :: UniqFM elt -> Bool
unitUFM         :: Uniquable key => key -> elt -> UniqFM elt
unitDirectlyUFM -- got the Unique already
                :: Unique -> elt -> UniqFM elt
listToUFM       :: Uniquable key => [(key,elt)] -> UniqFM elt
listToUFM_Directly
                :: [(Unique, elt)] -> UniqFM elt
listToUFM_C     :: Uniquable key => (elt -> elt -> elt)
                           -> [(key, elt)]
                           -> UniqFM elt

addToUFM        :: Uniquable key => UniqFM elt -> key -> elt  -> UniqFM elt
addListToUFM    :: Uniquable key => UniqFM elt -> [(key,elt)] -> UniqFM elt
addListToUFM_Directly :: UniqFM elt -> [(Unique,elt)] -> UniqFM elt
addToUFM_Directly
                :: UniqFM elt -> Unique -> elt -> UniqFM elt

addToUFM_C      :: Uniquable key => (elt -> elt -> elt) -- old -> new -> result
                           -> UniqFM elt                -- old
                           -> key -> elt                -- new
                           -> UniqFM elt                -- result

addToUFM_Acc    :: Uniquable key =>
                              (elt -> elts -> elts)     -- Add to existing
                           -> (elt -> elts)             -- New element
                           -> UniqFM elts               -- old
                           -> key -> elt                -- new
                           -> UniqFM elts               -- result

alterUFM        :: Uniquable key =>
                              (Maybe elt -> Maybe elt)  -- How to adjust
                           -> UniqFM elt                -- old
                           -> key                       -- new
                           -> UniqFM elt                -- result

addListToUFM_C  :: Uniquable key => (elt -> elt -> elt)
                           -> UniqFM elt -> [(key,elt)]
                           -> UniqFM elt

adjustUFM       :: Uniquable key => (elt -> elt) -> UniqFM elt -> key -> UniqFM elt
adjustUFM_Directly :: (elt -> elt) -> UniqFM elt -> Unique -> UniqFM elt

delFromUFM      :: Uniquable key => UniqFM elt -> key    -> UniqFM elt
delListFromUFM  :: Uniquable key => UniqFM elt -> [key] -> UniqFM elt
delFromUFM_Directly :: UniqFM elt -> Unique -> UniqFM elt

-- Bindings in right argument shadow those in the left
plusUFM         :: UniqFM elt -> UniqFM elt -> UniqFM elt

plusUFM_C       :: (elt -> elt -> elt)
                -> UniqFM elt -> UniqFM elt -> UniqFM elt

minusUFM        :: UniqFM elt1 -> UniqFM elt2 -> UniqFM elt1

intersectUFM    :: UniqFM elt -> UniqFM elt -> UniqFM elt
intersectUFM_C  :: (elt1 -> elt2 -> elt3)
                -> UniqFM elt1 -> UniqFM elt2 -> UniqFM elt3

foldUFM         :: (elt -> a -> a) -> a -> UniqFM elt -> a
foldUFM_Directly:: (Unique -> elt -> a -> a) -> a -> UniqFM elt -> a
mapUFM          :: (elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
mapUFM_Directly :: (Unique -> elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
mapMaybeUFM     :: (elt1 -> Maybe elt2) -> UniqFM elt1 -> UniqFM elt2
filterUFM       :: (elt -> Bool) -> UniqFM elt -> UniqFM elt
filterUFM_Directly :: (Unique -> elt -> Bool) -> UniqFM elt -> UniqFM elt
partitionUFM    :: (elt -> Bool) -> UniqFM elt -> (UniqFM elt, UniqFM elt)

sizeUFM         :: UniqFM elt -> Int
--hashUFM               :: UniqFM elt -> Int
elemUFM         :: Uniquable key => key -> UniqFM elt -> Bool
elemUFM_Directly:: Unique -> UniqFM elt -> Bool

splitUFM        :: Uniquable key => UniqFM elt -> key -> (UniqFM elt, Maybe elt, UniqFM elt)
                   -- Splits a UFM into things less than, equal to, and greater than the key
lookupUFM       :: Uniquable key => UniqFM elt -> key -> Maybe elt
lookupUFM_Directly  -- when you've got the Unique already
                :: UniqFM elt -> Unique -> Maybe elt
lookupWithDefaultUFM
                :: Uniquable key => UniqFM elt -> elt -> key -> elt
lookupWithDefaultUFM_Directly
                :: UniqFM elt -> elt -> Unique -> elt
keysUFM         :: UniqFM elt -> [Unique]       -- Get the keys
eltsUFM         :: UniqFM elt -> [elt]
ufmToList       :: UniqFM elt -> [(Unique, elt)]

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Implementation using ``Data.IntMap''}
%*                                                                      *
%************************************************************************

\begin{code}
newtype UniqFM ele = UFM { unUFM :: M.IntMap ele }
  deriving (Typeable,Data, Traversable.Traversable, Functor)

instance Eq ele => Eq (UniqFM ele) where
    (==) = (==) `on` unUFM

{-
instance Functor UniqFM where
   fmap f = fmap f . unUFM

instance Traversable.Traversable UniqFM where
    traverse f = Traversable.traverse f . unUFM
-}

instance Foldable.Foldable UniqFM where
    foldMap f = Foldable.foldMap f . unUFM

emptyUFM = UFM M.empty
isNullUFM (UFM m) = M.null m
unitUFM k v = UFM (M.singleton (getKey $ getUnique k) v)
unitDirectlyUFM u v = UFM (M.singleton (getKey u) v)
listToUFM = foldl (\m (k, v) -> addToUFM m k v) emptyUFM
listToUFM_Directly = foldl (\m (u, v) -> addToUFM_Directly m u v) emptyUFM
listToUFM_C f = foldl (\m (k, v) -> addToUFM_C f m k v) emptyUFM

alterUFM f (UFM m) k = UFM (M.alter f (getKey $ getUnique k) m)
addToUFM (UFM m) k v   = UFM (M.insert (getKey $ getUnique k) v m)
addListToUFM = foldl (\m (k, v) -> addToUFM m k v)
addListToUFM_Directly = foldl (\m (k, v) -> addToUFM_Directly m k v)
addToUFM_Directly (UFM m) u v = UFM (M.insert (getKey u) v m)

-- Arguments of combining function of M.insertWith and addToUFM_C are flipped.
addToUFM_C f (UFM m) k v =
  UFM (M.insertWith (flip f) (getKey $ getUnique k) v m)
addToUFM_Acc exi new (UFM m) k v =
  UFM (M.insertWith (\_new old -> exi v old) (getKey $ getUnique k) (new v) m)
addListToUFM_C f = foldl (\m (k, v) -> addToUFM_C f m k v)

adjustUFM f (UFM m) k = UFM (M.adjust f (getKey $ getUnique k) m)
adjustUFM_Directly f (UFM m) u = UFM (M.adjust f (getKey u) m)

delFromUFM (UFM m) k = UFM (M.delete (getKey $ getUnique k) m)
delListFromUFM = foldl delFromUFM
delFromUFM_Directly (UFM m) u = UFM (M.delete (getKey u) m)

-- M.union is left-biased, plusUFM should be right-biased.
plusUFM (UFM x) (UFM y) = UFM (M.union y x)
plusUFM_C f (UFM x) (UFM y) = UFM (M.unionWith f x y)
minusUFM (UFM x) (UFM y) = UFM (M.difference x y)
intersectUFM (UFM x) (UFM y) = UFM (M.intersection x y)
intersectUFM_C f (UFM x) (UFM y) = UFM (M.intersectionWith f x y)

foldUFM k z (UFM m) = M.fold k z m
foldUFM_Directly k z (UFM m) = M.foldWithKey (k . getUnique) z m
mapUFM f (UFM m) = UFM (M.map f m)
mapUFM_Directly f (UFM m) = UFM (M.mapWithKey (f . getUnique) m)
mapMaybeUFM f (UFM m) = UFM (M.mapMaybe f m)
filterUFM p (UFM m) = UFM (M.filter p m)
filterUFM_Directly p (UFM m) = UFM (M.filterWithKey (p . getUnique) m)
partitionUFM p (UFM m) = case M.partition p m of
                           (left, right) -> (UFM left, UFM right)

sizeUFM (UFM m) = M.size m
elemUFM k (UFM m) = M.member (getKey $ getUnique k) m
elemUFM_Directly u (UFM m) = M.member (getKey u) m

splitUFM (UFM m) k = case M.splitLookup (getKey $ getUnique k) m of
                       (less, equal, greater) -> (UFM less, equal, UFM greater)
lookupUFM (UFM m) k = M.lookup (getKey $ getUnique k) m
lookupUFM_Directly (UFM m) u = M.lookup (getKey u) m
lookupWithDefaultUFM (UFM m) v k = M.findWithDefault v (getKey $ getUnique k) m
lookupWithDefaultUFM_Directly (UFM m) v u = M.findWithDefault v (getKey u) m
keysUFM (UFM m) = map getUnique $ M.keys m
eltsUFM (UFM m) = M.elems m
ufmToList (UFM m) = map (\(k, v) -> (getUnique k, v)) $ M.toList m

-- Hoopl
joinUFM :: JoinFun v -> JoinFun (UniqFM v)
joinUFM eltJoin l (OldFact old) (NewFact new) = foldUFM_Directly add (NoChange, old) new
    where add k new_v (ch, joinmap) =
            case lookupUFM_Directly joinmap k of
                Nothing -> (SomeChange, addToUFM_Directly joinmap k new_v)
                Just old_v -> case eltJoin l (OldFact old_v) (NewFact new_v) of
                                (SomeChange, v') -> (SomeChange, addToUFM_Directly joinmap k v')
                                (NoChange, _) -> (ch, joinmap)

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Output-ery}
%*                                                                      *
%************************************************************************

\begin{code}
instance Outputable a => Outputable (UniqFM a) where
    ppr ufm = ppr (ufmToList ufm)
\end{code}
