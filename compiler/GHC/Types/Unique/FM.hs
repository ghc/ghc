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
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module GHC.Types.Unique.FM (
        -- * Unique-keyed mappings
        UniqFM,           -- abstract type
        NonDetUniqFM(..), -- wrapper for opting into nondeterminism

        -- ** Manipulating those mappings
        emptyUFM,
        unitUFM,
        unitDirectlyUFM,
        zipToUFM,
        listToUFM,
        listToUFM_Directly,
        listToUFM_C,
        listToIdentityUFM,
        addToUFM,addToUFM_C,addToUFM_Acc,
        addListToUFM,addListToUFM_C,
        addToUFM_Directly,
        addListToUFM_Directly,
        adjustUFM, alterUFM,
        adjustManyUFM,
        adjustUFM_Directly,
        delFromUFM,
        delFromUFM_Directly,
        delListFromUFM,
        delListFromUFM_Directly,
        plusUFM,
        plusUFM_C,
        plusUFM_CD,
        plusUFM_CD2,
        mergeUFM,
        plusMaybeUFM_C,
        plusFilterUFM_C,
        plusUFMList,
        sequenceUFMList,
        minusUFM,
        minusUFM_C,
        intersectUFM,
        intersectUFM_C,
        disjointUFM,
        equalKeysUFM,
        diffUFM,
        nonDetStrictFoldUFM, foldUFM, nonDetStrictFoldUFM_DirectlyM,
        anyUFM, allUFM, seqEltsUFM,
        mapUFM, mapUFM_Directly,
        mapMaybeUFM,
        elemUFM, elemUFM_Directly,
        filterUFM, filterUFM_Directly, partitionUFM,
        sizeUFM,
        isNullUFM,
        lookupUFM, lookupUFM_Directly,
        lookupWithDefaultUFM, lookupWithDefaultUFM_Directly,
        nonDetEltsUFM, nonDetKeysUFM,
        ufmToSet_Directly,
        nonDetUFMToList, ufmToIntMap, unsafeIntMapToUFM,
        unsafeCastUFMKey,
        pprUniqFM, pprUFM, pprUFMWithKeys, pluralUFM
    ) where

import GHC.Prelude

import GHC.Types.Unique ( Uniquable(..), Unique, getKey )
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import qualified Data.IntMap as M
import qualified Data.IntMap.Merge.Strict as MS
import qualified Data.IntMap.Strict as MS
import qualified Data.IntSet as S
import Data.Data
import qualified Data.Semigroup as Semi
import Data.Functor.Classes (Eq1 (..))
import Data.Coerce

-- | A finite map from @uniques@ of one type to
-- elements in another type.
--
-- The key is just here to keep us honest. It's always safe
-- to use a single type as key.
-- If two types don't overlap in their uniques it's also safe
-- to index the same map at multiple key types. But this is
-- very much discouraged.
newtype UniqFM key ele = UFM (M.IntMap ele)
  deriving (Data, Eq, Functor)
  -- Nondeterministic Foldable and Traversable instances are accessible through
  -- use of the 'NonDetUniqFM' wrapper.
  -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM to learn about determinism.

emptyUFM :: UniqFM key elt
emptyUFM = UFM M.empty

isNullUFM :: UniqFM key elt -> Bool
isNullUFM (UFM m) = M.null m

unitUFM :: Uniquable key => key -> elt -> UniqFM key elt
unitUFM k v = UFM (M.singleton (getKey $ getUnique k) v)

-- when you've got the Unique already
unitDirectlyUFM :: Unique -> elt -> UniqFM key elt
unitDirectlyUFM u v = UFM (M.singleton (getKey u) v)

-- zipToUFM ks vs = listToUFM (zip ks vs)
-- This function exists because it's a common case (#18535), and
-- it's inefficient to first build a list of pairs, and then immediately
-- take it apart. Astonishingly, fusing this one list away reduces total
-- compiler allocation by more than 10% (in T12545, see !3935)
-- Note that listToUFM (zip ks vs) performs similarly, but
-- the explicit recursion avoids relying too much on fusion.
zipToUFM :: Uniquable key => [key] -> [elt] -> UniqFM key elt
zipToUFM ks vs = assert (length ks == length vs ) innerZip emptyUFM ks vs
  where
    innerZip ufm (k:kList) (v:vList) = innerZip (addToUFM ufm k v) kList vList
    innerZip ufm _ _ = ufm

listToUFM :: Uniquable key => [(key,elt)] -> UniqFM key elt
listToUFM = foldl' (\m (k, v) -> addToUFM m k v) emptyUFM

listToUFM_Directly :: [(Unique, elt)] -> UniqFM key elt
listToUFM_Directly = foldl' (\m (u, v) -> addToUFM_Directly m u v) emptyUFM

listToIdentityUFM :: Uniquable key => [key] -> UniqFM key key
listToIdentityUFM = foldl' (\m x -> addToUFM m x x) emptyUFM

listToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)
  -> [(key, elt)]
  -> UniqFM key elt
listToUFM_C f = foldl' (\m (k, v) -> addToUFM_C f m k v) emptyUFM

addToUFM :: Uniquable key => UniqFM key elt -> key -> elt  -> UniqFM key elt
addToUFM (UFM m) k v = UFM (M.insert (getKey $ getUnique k) v m)

addListToUFM :: Uniquable key => UniqFM key elt -> [(key,elt)] -> UniqFM key elt
addListToUFM = foldl' (\m (k, v) -> addToUFM m k v)

addListToUFM_Directly :: UniqFM key elt -> [(Unique,elt)] -> UniqFM key elt
addListToUFM_Directly = foldl' (\m (k, v) -> addToUFM_Directly m k v)

addToUFM_Directly :: UniqFM key elt -> Unique -> elt -> UniqFM key elt
addToUFM_Directly (UFM m) u v = UFM (M.insert (getKey u) v m)

addToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)  -- old -> new -> result
  -> UniqFM key elt           -- old
  -> key -> elt           -- new
  -> UniqFM key elt           -- result
-- Arguments of combining function of M.insertWith and addToUFM_C are flipped.
addToUFM_C f (UFM m) k v =
  UFM (M.insertWith (flip f) (getKey $ getUnique k) v m)

addToUFM_Acc
  :: Uniquable key
  => (elt -> elts -> elts)  -- Add to existing
  -> (elt -> elts)          -- New element
  -> UniqFM key elts            -- old
  -> key -> elt             -- new
  -> UniqFM key elts            -- result
addToUFM_Acc exi new (UFM m) k v =
  UFM (M.insertWith (\_new old -> exi v old) (getKey $ getUnique k) (new v) m)

alterUFM
  :: Uniquable key
  => (Maybe elt -> Maybe elt)  -- How to adjust
  -> UniqFM key elt                -- old
  -> key                       -- new
  -> UniqFM key elt                -- result
alterUFM f (UFM m) k = UFM (M.alter f (getKey $ getUnique k) m)

-- | Add elements to the map, combining existing values with inserted ones using
-- the given function.
addListToUFM_C
  :: Uniquable key
  => (elt -> elt -> elt)
  -> UniqFM key elt -> [(key,elt)]
  -> UniqFM key elt
addListToUFM_C f = foldl' (\m (k, v) -> addToUFM_C f m k v)

adjustUFM :: Uniquable key => (elt -> elt) -> UniqFM key elt -> key -> UniqFM key elt
adjustUFM f (UFM m) k = UFM (M.adjust f (getKey $ getUnique k) m)

adjustUFM_Directly :: (elt -> elt) -> UniqFM key elt -> Unique -> UniqFM key elt
adjustUFM_Directly f (UFM m) u = UFM (M.adjust f (getKey u) m)

adjustManyUFM :: Uniquable key => (elt -> elt) -> UniqFM key elt -> UniqFM key any -> UniqFM key elt
adjustManyUFM f (UFM m) (UFM keys)
  = UFM $ MS.merge
      MS.preserveMissing -- preserve all entries in m that aren't in keys
      MS.dropMissing     -- drop all entries in keys that aren't in m
      (MS.zipWithMatched $ \_key elt _any -> f elt) -- apply f to common keys
      m keys

delFromUFM :: Uniquable key => UniqFM key elt -> key    -> UniqFM key elt
delFromUFM (UFM m) k = UFM (M.delete (getKey $ getUnique k) m)

delListFromUFM :: Uniquable key => UniqFM key elt -> [key] -> UniqFM key elt
delListFromUFM = foldl' delFromUFM

delListFromUFM_Directly :: UniqFM key elt -> [Unique] -> UniqFM key elt
delListFromUFM_Directly = foldl' delFromUFM_Directly

delFromUFM_Directly :: UniqFM key elt -> Unique -> UniqFM key elt
delFromUFM_Directly (UFM m) u = UFM (M.delete (getKey u) m)

-- Bindings in right argument shadow those in the left
plusUFM :: UniqFM key elt -> UniqFM key elt -> UniqFM key elt
-- M.union is left-biased, plusUFM should be right-biased.
plusUFM (UFM x) (UFM y) = UFM (M.union y x)
     -- Note (M.union y x), with arguments flipped
     -- M.union is left-biased, plusUFM should be right-biased.

plusUFM_C :: (elt -> elt -> elt) -> UniqFM key elt -> UniqFM key elt -> UniqFM key elt
plusUFM_C f (UFM x) (UFM y) = UFM (M.unionWith f x y)

-- | `plusUFM_CD f m1 d1 m2 d2` merges the maps using `f` as the
-- combinding function and `d1` resp. `d2` as the default value if
-- there is no entry in `m1` reps. `m2`. The domain is the union of
-- the domains of `m1` and `m2`.
--
-- IMPORTANT NOTE: This function strictly applies the modification function
-- and forces the result unlike most the other functions in this module.
--
-- Representative example:
--
-- @
-- plusUFM_CD f {A: 1, B: 2} 23 {B: 3, C: 4} 42
--    == {A: f 1 42, B: f 2 3, C: f 23 4 }
-- @
{-# INLINE plusUFM_CD #-}
plusUFM_CD
  :: (elta -> eltb -> eltc)
  -> UniqFM key elta  -- map X
  -> elta         -- default for X
  -> UniqFM key eltb  -- map Y
  -> eltb         -- default for Y
  -> UniqFM key eltc
plusUFM_CD f (UFM xm) dx (UFM ym) dy
  = UFM $ MS.mergeWithKey
      (\_ x y -> Just (x `f` y))
      (MS.map (\x -> x `f` dy))
      (MS.map (\y -> dx `f` y))
      xm ym

-- | `plusUFM_CD2 f m1 m2` merges the maps using `f` as the combining
-- function. Unlike `plusUFM_CD`, a missing value is not defaulted: it is
-- instead passed as `Nothing` to `f`. `f` can never have both its arguments
-- be `Nothing`.
--
-- IMPORTANT NOTE: This function strictly applies the modification function
-- and forces the result.
--
-- `plusUFM_CD2 f m1 m2` is the same as `plusUFM_CD f (mapUFM Just m1) Nothing
-- (mapUFM Just m2) Nothing`.
plusUFM_CD2
  :: (Maybe elta -> Maybe eltb -> eltc)
  -> UniqFM key elta  -- map X
  -> UniqFM key eltb  -- map Y
  -> UniqFM key eltc
plusUFM_CD2 f (UFM xm) (UFM ym)
  = UFM $ MS.mergeWithKey
      (\_ x y -> Just (Just x `f` Just y))
      (MS.map (\x -> Just x `f` Nothing))
      (MS.map (\y -> Nothing `f` Just y))
      xm ym

mergeUFM
  :: (elta -> eltb -> Maybe eltc)
  -> (UniqFM key elta -> UniqFM key eltc)  -- map X
  -> (UniqFM key eltb -> UniqFM key eltc) -- map Y
  -> UniqFM key elta
  -> UniqFM key eltb
  -> UniqFM key eltc
mergeUFM f g h (UFM xm) (UFM ym)
  = UFM $ MS.mergeWithKey
      (\_ x y -> (x `f` y))
      (coerce g)
      (coerce h)
      xm ym

plusMaybeUFM_C :: (elt -> elt -> Maybe elt)
               -> UniqFM key elt -> UniqFM key elt -> UniqFM key elt
plusMaybeUFM_C f (UFM xm) (UFM ym)
    = UFM $ M.mergeWithKey
        (\_ x y -> x `f` y)
        id
        id
        xm ym

-- | @plusFilterUFM_C f p l r@ is like @'plusUFM_C' f l r@, but filters the
-- entries /that only occur/ in @r@ by @p@ before.
plusFilterUFM_C
  :: (elt -> elt -> elt)
  -> (elt -> Bool)
  -> UniqFM key elt
  -> UniqFM key elt
  -> UniqFM key elt
plusFilterUFM_C f p (UFM l) (UFM r)
  = UFM $ MS.merge
      MS.preserveMissing                -- preserve entries in l that aren't in r
      (MS.filterMissing $ \_k x -> p x) -- filter entries in r by p
      (MS.zipWithMatched $ \_k a b -> f a b) -- apply f to common keys
      l r

plusUFMList :: [UniqFM key elt] -> UniqFM key elt
plusUFMList = foldl' plusUFM emptyUFM

sequenceUFMList :: forall key elt. [UniqFM key elt] -> UniqFM key [elt]
sequenceUFMList = foldr (plusUFM_CD2 cons) emptyUFM
  where
    cons :: Maybe elt -> Maybe [elt] -> [elt]
    cons (Just x) (Just ys) = x : ys
    cons Nothing  (Just ys) = ys
    cons (Just x) Nothing   = [x]
    cons Nothing  Nothing   = []

minusUFM :: UniqFM key elt1 -> UniqFM key elt2 -> UniqFM key elt1
minusUFM (UFM x) (UFM y) = UFM (M.difference x y)

-- | @minusUFC_C f map1 map2@ returns @map1@, except that every mapping @key
-- |-> value1@ in @map1@ that shares a key with a mapping @key |-> value2@ in
-- @map2@ is altered by @f@: @value1@ is replaced by @f value1 value2@, where
-- 'Just' means that the new value is used and 'Nothing' means that the mapping
-- is deleted.
minusUFM_C :: (elt1 -> elt2 -> Maybe elt1) -> UniqFM key elt1 -> UniqFM key elt2 -> UniqFM key elt1
minusUFM_C f (UFM x) (UFM y) = UFM (M.differenceWith f x y)

intersectUFM :: UniqFM key elt1 -> UniqFM key elt2 -> UniqFM key elt1
intersectUFM (UFM x) (UFM y) = UFM (M.intersection x y)

intersectUFM_C
  :: (elt1 -> elt2 -> elt3)
  -> UniqFM key elt1
  -> UniqFM key elt2
  -> UniqFM key elt3
intersectUFM_C f (UFM x) (UFM y) = UFM (M.intersectionWith f x y)

disjointUFM :: UniqFM key elt1 -> UniqFM key elt2 -> Bool
disjointUFM (UFM x) (UFM y) = M.disjoint x y

foldUFM :: (elt -> a -> a) -> a -> UniqFM key elt -> a
foldUFM k z (UFM m) = M.foldr k z m

mapUFM :: (elt1 -> elt2) -> UniqFM key elt1 -> UniqFM key elt2
mapUFM f (UFM m) = UFM (M.map f m)

mapMaybeUFM :: (elt1 -> Maybe elt2) -> UniqFM key elt1 -> UniqFM key elt2
mapMaybeUFM f (UFM m) = UFM (M.mapMaybe f m)

mapUFM_Directly :: (Unique -> elt1 -> elt2) -> UniqFM key elt1 -> UniqFM key elt2
mapUFM_Directly f (UFM m) = UFM (M.mapWithKey (f . getUnique) m)

filterUFM :: (elt -> Bool) -> UniqFM key elt -> UniqFM key elt
filterUFM p (UFM m) = UFM (M.filter p m)

filterUFM_Directly :: (Unique -> elt -> Bool) -> UniqFM key elt -> UniqFM key elt
filterUFM_Directly p (UFM m) = UFM (M.filterWithKey (p . getUnique) m)

partitionUFM :: (elt -> Bool) -> UniqFM key elt -> (UniqFM key elt, UniqFM key elt)
partitionUFM p (UFM m) =
  case M.partition p m of
    (left, right) -> (UFM left, UFM right)

sizeUFM :: UniqFM key elt -> Int
sizeUFM (UFM m) = M.size m

elemUFM :: Uniquable key => key -> UniqFM key elt -> Bool
elemUFM k (UFM m) = M.member (getKey $ getUnique k) m

elemUFM_Directly :: Unique -> UniqFM key elt -> Bool
elemUFM_Directly u (UFM m) = M.member (getKey u) m

lookupUFM :: Uniquable key => UniqFM key elt -> key -> Maybe elt
lookupUFM (UFM m) k = M.lookup (getKey $ getUnique k) m

-- when you've got the Unique already
lookupUFM_Directly :: UniqFM key elt -> Unique -> Maybe elt
lookupUFM_Directly (UFM m) u = M.lookup (getKey u) m

lookupWithDefaultUFM :: Uniquable key => UniqFM key elt -> elt -> key -> elt
lookupWithDefaultUFM (UFM m) v k = M.findWithDefault v (getKey $ getUnique k) m

lookupWithDefaultUFM_Directly :: UniqFM key elt -> elt -> Unique -> elt
lookupWithDefaultUFM_Directly (UFM m) v u = M.findWithDefault v (getKey u) m

ufmToSet_Directly :: UniqFM key elt -> S.IntSet
ufmToSet_Directly (UFM m) = M.keysSet m

anyUFM :: (elt -> Bool) -> UniqFM key elt -> Bool
anyUFM p (UFM m) = M.foldr ((||) . p) False m

allUFM :: (elt -> Bool) -> UniqFM key elt -> Bool
allUFM p (UFM m) = M.foldr ((&&) . p) True m

seqEltsUFM :: (elt -> ()) -> UniqFM key elt -> ()
seqEltsUFM seqElt = foldUFM (\v rest -> seqElt v `seq` rest) ()

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetEltsUFM :: UniqFM key elt -> [elt]
nonDetEltsUFM (UFM m) = M.elems m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetKeysUFM :: UniqFM key elt -> [Unique]
nonDetKeysUFM (UFM m) = map getUnique $ M.keys m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldUFM :: (elt -> a -> a) -> a -> UniqFM key elt -> a
nonDetStrictFoldUFM k z (UFM m) = M.foldl' (flip k) z m

-- | In essence foldM
-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
{-# INLINE nonDetStrictFoldUFM_DirectlyM #-} -- Allow specialization
nonDetStrictFoldUFM_DirectlyM :: (Monad m) => (Unique -> b -> elt -> m b) -> b -> UniqFM key elt -> m b
nonDetStrictFoldUFM_DirectlyM f z0 (UFM xs) = M.foldrWithKey c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c u x k z = f (getUnique u) z x >>= k
        {-# INLINE c #-}

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetUFMToList :: UniqFM key elt -> [(Unique, elt)]
nonDetUFMToList (UFM m) = map (\(k, v) -> (getUnique k, v)) $ M.toList m

-- | A wrapper around 'UniqFM' with the sole purpose of informing call sites
-- that the provided 'Foldable' and 'Traversable' instances are
-- nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" to learn about determinism.
newtype NonDetUniqFM key ele = NonDetUniqFM { getNonDet :: UniqFM key ele }
  deriving (Functor)

-- | Inherently nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" to learn about determinism.
instance forall key. Foldable (NonDetUniqFM key) where
  foldr f z (NonDetUniqFM (UFM m)) = foldr f z m

-- | Inherently nondeterministic.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" to learn about determinism.
instance forall key. Traversable (NonDetUniqFM key) where
  traverse f (NonDetUniqFM (UFM m)) = NonDetUniqFM . UFM <$> traverse f m

ufmToIntMap :: UniqFM key elt -> M.IntMap elt
ufmToIntMap (UFM m) = m

unsafeIntMapToUFM :: M.IntMap elt -> UniqFM key elt
unsafeIntMapToUFM = UFM

-- | Cast the key domain of a UniqFM.
--
-- As long as the domains don't overlap in their uniques
-- this is safe.
unsafeCastUFMKey :: UniqFM key1 elt -> UniqFM key2 elt
unsafeCastUFMKey (UFM m) = UFM m

-- Determines whether two 'UniqFM's contain the same keys.
equalKeysUFM :: UniqFM key a -> UniqFM key b -> Bool
equalKeysUFM (UFM m1) (UFM m2) = liftEq (\_ _ -> True) m1 m2

-- | An edit on type @a@, relating an element of a container (like an entry in a
-- map or a line in a file) before and after.
data Edit a
  = Removed !a    -- ^ Element was removed from the container
  | Added !a      -- ^ Element was added to the container
  | Changed !a !a -- ^ Element was changed. Carries the values before and after
  deriving Eq

instance Outputable a => Outputable (Edit a) where
  ppr (Removed a) = text "-" <> ppr a
  ppr (Added a) = text "+" <> ppr a
  ppr (Changed l r) = ppr l <> text "->" <> ppr r

-- A very convient function to have for debugging:
-- | Computes the diff of two 'UniqFM's in terms of 'Edit's.
-- Equal points will not be present in the result map at all.
diffUFM :: Eq a => UniqFM key a -> UniqFM key a -> UniqFM key (Edit a)
diffUFM = mergeUFM both (mapUFM Removed) (mapUFM Added)
  where
    both x y | x == y    = Nothing
             | otherwise = Just $! Changed x y

-- Instances

instance Semi.Semigroup (UniqFM key a) where
  (<>) = plusUFM

instance Monoid (UniqFM key a) where
    mempty = emptyUFM
    mappend = (Semi.<>)

-- Output-ery

instance Outputable a => Outputable (UniqFM key a) where
    ppr ufm = pprUniqFM ppr ufm

pprUniqFM :: (a -> SDoc) -> UniqFM key a -> SDoc
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
pprUFM :: UniqFM key a      -- ^ The things to be pretty printed
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
       :: UniqFM key a                -- ^ The things to be pretty printed
       -> ([(Unique, a)] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc                    -- ^ 'SDoc' where the things have been pretty
                                  -- printed
pprUFMWithKeys ufm pp = pp (nonDetUFMToList ufm)

-- | Determines the pluralisation suffix appropriate for the length of a set
-- in the same way that plural from Outputable does for lists.
pluralUFM :: UniqFM key a -> SDoc
pluralUFM ufm
  | sizeUFM ufm == 1 = empty
  | otherwise = char 's'
