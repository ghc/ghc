{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


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
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module GHC.Types.Unique.FM (
        -- * Unique-keyed mappings
        UniqFM,           -- abstract type
        NonDetUniqFM(..), -- wrapper for opting into nondeterminism

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
        delListFromUFM_Directly,
        plusUFM,
        plusUFM_C,
        plusUFM_CD,
        plusMaybeUFM_C,
        plusUFMList,
        minusUFM,
        intersectUFM,
        intersectUFM_C,
        disjointUFM,
        equalKeysUFM,
        nonDetFoldUFM, foldUFM, nonDetFoldUFM_Directly,
        anyUFM, allUFM, seqEltsUFM,
        mapUFM, mapUFM_Directly,
        elemUFM, elemUFM_Directly,
        filterUFM, filterUFM_Directly, partitionUFM,
        sizeUFM,
        isNullUFM,
        lookupUFM, lookupUFM_Directly,
        lookupWithDefaultUFM, lookupWithDefaultUFM_Directly,
        nonDetEltsUFM, eltsUFM, nonDetKeysUFM,
        ufmToSet_Directly,
        nonDetUFMToList, ufmToIntMap,
        pprUniqFM, pprUFM, pprUFMWithKeys, pluralUFM
    ) where

import GhcPrelude

import GHC.Types.Unique ( Uniquable(..), Unique, getKey )
import Outputable

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.Data
import qualified Data.Semigroup as Semi
import Data.Functor.Classes (Eq1 (..))


newtype UniqFM ele = UFM (M.IntMap ele)
  deriving (Data, Eq, Functor)
  -- Nondeterministic Foldable and Traversable instances are accessible through
  -- use of the 'NonDetUniqFM' wrapper.
  -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM to learn about determinism.

emptyUFM :: UniqFM elt
emptyUFM = UFM M.empty

isNullUFM :: UniqFM elt -> Bool
isNullUFM (UFM m) = M.null m

unitUFM :: Uniquable key => key -> elt -> UniqFM elt
unitUFM k v = UFM (M.singleton (getKey $ getUnique k) v)

-- when you've got the Unique already
unitDirectlyUFM :: Unique -> elt -> UniqFM elt
unitDirectlyUFM u v = UFM (M.singleton (getKey u) v)

listToUFM :: Uniquable key => [(key,elt)] -> UniqFM elt
listToUFM = foldl' (\m (k, v) -> addToUFM m k v) emptyUFM

listToUFM_Directly :: [(Unique, elt)] -> UniqFM elt
listToUFM_Directly = foldl' (\m (u, v) -> addToUFM_Directly m u v) emptyUFM

listToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)
  -> [(key, elt)]
  -> UniqFM elt
listToUFM_C f = foldl' (\m (k, v) -> addToUFM_C f m k v) emptyUFM

addToUFM :: Uniquable key => UniqFM elt -> key -> elt  -> UniqFM elt
addToUFM (UFM m) k v = UFM (M.insert (getKey $ getUnique k) v m)

addListToUFM :: Uniquable key => UniqFM elt -> [(key,elt)] -> UniqFM elt
addListToUFM = foldl' (\m (k, v) -> addToUFM m k v)

addListToUFM_Directly :: UniqFM elt -> [(Unique,elt)] -> UniqFM elt
addListToUFM_Directly = foldl' (\m (k, v) -> addToUFM_Directly m k v)

addToUFM_Directly :: UniqFM elt -> Unique -> elt -> UniqFM elt
addToUFM_Directly (UFM m) u v = UFM (M.insert (getKey u) v m)

addToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)  -- old -> new -> result
  -> UniqFM elt           -- old
  -> key -> elt           -- new
  -> UniqFM elt           -- result
-- Arguments of combining function of M.insertWith and addToUFM_C are flipped.
addToUFM_C f (UFM m) k v =
  UFM (M.insertWith (flip f) (getKey $ getUnique k) v m)

addToUFM_Acc
  :: Uniquable key
  => (elt -> elts -> elts)  -- Add to existing
  -> (elt -> elts)          -- New element
  -> UniqFM elts            -- old
  -> key -> elt             -- new
  -> UniqFM elts            -- result
addToUFM_Acc exi new (UFM m) k v =
  UFM (M.insertWith (\_new old -> exi v old) (getKey $ getUnique k) (new v) m)

alterUFM
  :: Uniquable key
  => (Maybe elt -> Maybe elt)  -- How to adjust
  -> UniqFM elt                -- old
  -> key                       -- new
  -> UniqFM elt                -- result
alterUFM f (UFM m) k = UFM (M.alter f (getKey $ getUnique k) m)

addListToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)
  -> UniqFM elt -> [(key,elt)]
  -> UniqFM elt
addListToUFM_C f = foldl' (\m (k, v) -> addToUFM_C f m k v)

adjustUFM :: Uniquable key => (elt -> elt) -> UniqFM elt -> key -> UniqFM elt
adjustUFM f (UFM m) k = UFM (M.adjust f (getKey $ getUnique k) m)

adjustUFM_Directly :: (elt -> elt) -> UniqFM elt -> Unique -> UniqFM elt
adjustUFM_Directly f (UFM m) u = UFM (M.adjust f (getKey u) m)

delFromUFM :: Uniquable key => UniqFM elt -> key    -> UniqFM elt
delFromUFM (UFM m) k = UFM (M.delete (getKey $ getUnique k) m)

delListFromUFM :: Uniquable key => UniqFM elt -> [key] -> UniqFM elt
delListFromUFM = foldl' delFromUFM

delListFromUFM_Directly :: UniqFM elt -> [Unique] -> UniqFM elt
delListFromUFM_Directly = foldl' delFromUFM_Directly

delFromUFM_Directly :: UniqFM elt -> Unique -> UniqFM elt
delFromUFM_Directly (UFM m) u = UFM (M.delete (getKey u) m)

-- Bindings in right argument shadow those in the left
plusUFM :: UniqFM elt -> UniqFM elt -> UniqFM elt
-- M.union is left-biased, plusUFM should be right-biased.
plusUFM (UFM x) (UFM y) = UFM (M.union y x)
     -- Note (M.union y x), with arguments flipped
     -- M.union is left-biased, plusUFM should be right-biased.

plusUFM_C :: (elt -> elt -> elt) -> UniqFM elt -> UniqFM elt -> UniqFM elt
plusUFM_C f (UFM x) (UFM y) = UFM (M.unionWith f x y)

-- | `plusUFM_CD f m1 d1 m2 d2` merges the maps using `f` as the
-- combinding function and `d1` resp. `d2` as the default value if
-- there is no entry in `m1` reps. `m2`. The domain is the union of
-- the domains of `m1` and `m2`.
--
-- Representative example:
--
-- @
-- plusUFM_CD f {A: 1, B: 2} 23 {B: 3, C: 4} 42
--    == {A: f 1 42, B: f 2 3, C: f 23 4 }
-- @
plusUFM_CD
  :: (elt -> elt -> elt)
  -> UniqFM elt  -- map X
  -> elt         -- default for X
  -> UniqFM elt  -- map Y
  -> elt         -- default for Y
  -> UniqFM elt
plusUFM_CD f (UFM xm) dx (UFM ym) dy
  = UFM $ M.mergeWithKey
      (\_ x y -> Just (x `f` y))
      (M.map (\x -> x `f` dy))
      (M.map (\y -> dx `f` y))
      xm ym

plusMaybeUFM_C :: (elt -> elt -> Maybe elt)
               -> UniqFM elt -> UniqFM elt -> UniqFM elt
plusMaybeUFM_C f (UFM xm) (UFM ym)
    = UFM $ M.mergeWithKey
        (\_ x y -> x `f` y)
        id
        id
        xm ym

plusUFMList :: [UniqFM elt] -> UniqFM elt
plusUFMList = foldl' plusUFM emptyUFM

minusUFM :: UniqFM elt1 -> UniqFM elt2 -> UniqFM elt1
minusUFM (UFM x) (UFM y) = UFM (M.difference x y)

intersectUFM :: UniqFM elt1 -> UniqFM elt2 -> UniqFM elt1
intersectUFM (UFM x) (UFM y) = UFM (M.intersection x y)

intersectUFM_C
  :: (elt1 -> elt2 -> elt3)
  -> UniqFM elt1
  -> UniqFM elt2
  -> UniqFM elt3
intersectUFM_C f (UFM x) (UFM y) = UFM (M.intersectionWith f x y)

disjointUFM :: UniqFM elt1 -> UniqFM elt2 -> Bool
disjointUFM (UFM x) (UFM y) = M.disjoint x y

foldUFM :: (elt -> a -> a) -> a -> UniqFM elt -> a
foldUFM k z (UFM m) = M.foldr k z m

mapUFM :: (elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
mapUFM f (UFM m) = UFM (M.map f m)

mapUFM_Directly :: (Unique -> elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
mapUFM_Directly f (UFM m) = UFM (M.mapWithKey (f . getUnique) m)

filterUFM :: (elt -> Bool) -> UniqFM elt -> UniqFM elt
filterUFM p (UFM m) = UFM (M.filter p m)

filterUFM_Directly :: (Unique -> elt -> Bool) -> UniqFM elt -> UniqFM elt
filterUFM_Directly p (UFM m) = UFM (M.filterWithKey (p . getUnique) m)

partitionUFM :: (elt -> Bool) -> UniqFM elt -> (UniqFM elt, UniqFM elt)
partitionUFM p (UFM m) =
  case M.partition p m of
    (left, right) -> (UFM left, UFM right)

sizeUFM :: UniqFM elt -> Int
sizeUFM (UFM m) = M.size m

elemUFM :: Uniquable key => key -> UniqFM elt -> Bool
elemUFM k (UFM m) = M.member (getKey $ getUnique k) m

elemUFM_Directly :: Unique -> UniqFM elt -> Bool
elemUFM_Directly u (UFM m) = M.member (getKey u) m

lookupUFM :: Uniquable key => UniqFM elt -> key -> Maybe elt
lookupUFM (UFM m) k = M.lookup (getKey $ getUnique k) m

-- when you've got the Unique already
lookupUFM_Directly :: UniqFM elt -> Unique -> Maybe elt
lookupUFM_Directly (UFM m) u = M.lookup (getKey u) m

lookupWithDefaultUFM :: Uniquable key => UniqFM elt -> elt -> key -> elt
lookupWithDefaultUFM (UFM m) v k = M.findWithDefault v (getKey $ getUnique k) m

lookupWithDefaultUFM_Directly :: UniqFM elt -> elt -> Unique -> elt
lookupWithDefaultUFM_Directly (UFM m) v u = M.findWithDefault v (getKey u) m

eltsUFM :: UniqFM elt -> [elt]
eltsUFM (UFM m) = M.elems m

ufmToSet_Directly :: UniqFM elt -> S.IntSet
ufmToSet_Directly (UFM m) = M.keysSet m

anyUFM :: (elt -> Bool) -> UniqFM elt -> Bool
anyUFM p (UFM m) = M.foldr ((||) . p) False m

allUFM :: (elt -> Bool) -> UniqFM elt -> Bool
allUFM p (UFM m) = M.foldr ((&&) . p) True m

seqEltsUFM :: ([elt] -> ()) -> UniqFM elt -> ()
seqEltsUFM seqList = seqList . nonDetEltsUFM
  -- It's OK to use nonDetEltsUFM here because the type guarantees that
  -- the only interesting thing this function can do is to force the
  -- elements.

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetEltsUFM :: UniqFM elt -> [elt]
nonDetEltsUFM (UFM m) = M.elems m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetKeysUFM :: UniqFM elt -> [Unique]
nonDetKeysUFM (UFM m) = map getUnique $ M.keys m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUFM :: (elt -> a -> a) -> a -> UniqFM elt -> a
nonDetFoldUFM k z (UFM m) = M.foldr k z m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUFM_Directly:: (Unique -> elt -> a -> a) -> a -> UniqFM elt -> a
nonDetFoldUFM_Directly k z (UFM m) = M.foldrWithKey (k . getUnique) z m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetUFMToList :: UniqFM elt -> [(Unique, elt)]
nonDetUFMToList (UFM m) = map (\(k, v) -> (getUnique k, v)) $ M.toList m

-- | A wrapper around 'UniqFM' with the sole purpose of informing call sites
-- that the provided 'Foldable' and 'Traversable' instances are
-- nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM to learn about determinism.
newtype NonDetUniqFM ele = NonDetUniqFM { getNonDet :: UniqFM ele }
  deriving (Functor)

-- | Inherently nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM to learn about determinism.
instance Foldable NonDetUniqFM where
  foldr f z (NonDetUniqFM (UFM m)) = foldr f z m

-- | Inherently nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM to learn about determinism.
instance Traversable NonDetUniqFM where
  traverse f (NonDetUniqFM (UFM m)) = NonDetUniqFM . UFM <$> traverse f m

ufmToIntMap :: UniqFM elt -> M.IntMap elt
ufmToIntMap (UFM m) = m

-- Determines whether two 'UniqFM's contain the same keys.
equalKeysUFM :: UniqFM a -> UniqFM b -> Bool
equalKeysUFM (UFM m1) (UFM m2) = liftEq (\_ _ -> True) m1 m2

-- Instances

instance Semi.Semigroup (UniqFM a) where
  (<>) = plusUFM

instance Monoid (UniqFM a) where
    mempty = emptyUFM
    mappend = (Semi.<>)

-- Output-ery

instance Outputable a => Outputable (UniqFM a) where
    ppr ufm = pprUniqFM ppr ufm

pprUniqFM :: (a -> SDoc) -> UniqFM a -> SDoc
pprUniqFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt elt
    | (uq, elt) <- nonDetUFMToList ufm ]
  -- It's OK to use nonDetUFMToList here because we only use it for
  -- pretty-printing.

-- | Pretty-print a non-deterministic set.
-- The order of variables is non-deterministic and for pretty-printing that
-- shouldn't be a problem.
-- Having this function helps contain the non-determinism created with
-- nonDetEltsUFM.
pprUFM :: UniqFM a      -- ^ The things to be pretty printed
       -> ([a] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc          -- ^ 'SDoc' where the things have been pretty
                        -- printed
pprUFM ufm pp = pp (nonDetEltsUFM ufm)

-- | Pretty-print a non-deterministic set.
-- The order of variables is non-deterministic and for pretty-printing that
-- shouldn't be a problem.
-- Having this function helps contain the non-determinism created with
-- nonDetUFMToList.
pprUFMWithKeys
       :: UniqFM a                -- ^ The things to be pretty printed
       -> ([(Unique, a)] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc                    -- ^ 'SDoc' where the things have been pretty
                                  -- printed
pprUFMWithKeys ufm pp = pp (nonDetUFMToList ufm)

-- | Determines the pluralisation suffix appropriate for the length of a set
-- in the same way that plural from Outputable does for lists.
pluralUFM :: UniqFM a -> SDoc
pluralUFM ufm
  | sizeUFM ufm == 1 = empty
  | otherwise = char 's'
