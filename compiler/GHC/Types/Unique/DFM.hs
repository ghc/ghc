{-
(c) Bartosz Nitka, Facebook, 2015

UniqDFM: Specialised deterministic finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

This is very similar to @UniqFM@, the major difference being that the order of
folding is not dependent on @Unique@ ordering, giving determinism.
Currently the ordering is determined by insertion order.

See Note [Unique Determinism] in GHC.Types.Unique for explanation why @Unique@ ordering
is not deterministic.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module GHC.Types.Unique.DFM (
        -- * Unique-keyed deterministic mappings
        UniqDFM,       -- abstract type

        -- ** Manipulating those mappings
        emptyUDFM,
        unitUDFM,
        addToUDFM,
        addToUDFM_C,
        addToUDFM_C_Directly,
        addToUDFM_Directly,
        addListToUDFM,
        delFromUDFM,
        delListFromUDFM,
        adjustUDFM,
        adjustUDFM_Directly,
        alterUDFM,
        mapUDFM,
        mapMaybeUDFM,
        plusUDFM,
        plusUDFM_C,
        lookupUDFM, lookupUDFM_Directly,
        elemUDFM,
        foldUDFM,
        eltsUDFM,
        filterUDFM, filterUDFM_Directly,
        isNullUDFM,
        sizeUDFM,
        intersectUDFM, udfmIntersectUFM,
        disjointUDFM, disjointUdfmUfm,
        equalKeysUDFM,
        minusUDFM,
        listToUDFM, listToUDFM_Directly,
        udfmMinusUFM, ufmMinusUDFM,
        partitionUDFM,
        anyUDFM, allUDFM,
        pprUniqDFM, pprUDFM,

        udfmToList,
        udfmToUfm,
        nonDetStrictFoldUDFM,
        unsafeCastUDFMKey,
        alwaysUnsafeUfmToUdfm,
    ) where

import GHC.Prelude

import GHC.Types.Unique ( Uniquable(..), Unique, getKey )
import GHC.Utils.Outputable

import qualified Data.IntMap.Strict as MS
import qualified Data.IntMap as M
import Data.Data
import Data.Functor.Classes (Eq1 (..))
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Semigroup as Semi
import GHC.Types.Unique.FM (UniqFM, nonDetUFMToList, ufmToIntMap, unsafeIntMapToUFM)
import Unsafe.Coerce

-- Note [Deterministic UniqFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A @UniqDFM@ is just like @UniqFM@ with the following additional
-- property: the function `udfmToList` returns the elements in some
-- deterministic order not depending on the Unique key for those elements.
--
-- If the client of the map performs operations on the map in deterministic
-- order then `udfmToList` returns them in deterministic order.
--
-- There is an implementation cost: each element is given a serial number
-- as it is added, and `udfmToList` sorts it's result by this serial
-- number. So you should only use `UniqDFM` if you need the deterministic
-- property.
--
-- `foldUDFM` also preserves determinism.
--
-- Normal @UniqFM@ when you turn it into a list will use
-- Data.IntMap.toList function that returns the elements in the order of
-- the keys. The keys in @UniqFM@ are always @Uniques@, so you end up with
-- with a list ordered by @Uniques@.
-- The order of @Uniques@ is known to be not stable across rebuilds.
-- See Note [Unique Determinism] in GHC.Types.Unique.
--
--
-- There's more than one way to implement this. The implementation here tags
-- every value with the insertion time that can later be used to sort the
-- values when asked to convert to a list.
--
-- An alternative would be to have
--
--   data UniqDFM ele = UDFM (M.IntMap ele) [ele]
--
-- where the list determines the order. This makes deletion tricky as we'd
-- only accumulate elements in that list, but makes merging easier as you
-- can just merge both structures independently.
-- Deletion can probably be done in amortized fashion when the size of the
-- list is twice the size of the set.

-- | A type of values tagged with insertion time
data TaggedVal val =
  TaggedVal
    !val
    {-# UNPACK #-} !Int -- ^ insertion time
  deriving stock (Data, Functor, Foldable, Traversable)

taggedFst :: TaggedVal val -> val
taggedFst (TaggedVal v _) = v

taggedSnd :: TaggedVal val -> Int
taggedSnd (TaggedVal _ i) = i

instance Eq val => Eq (TaggedVal val) where
  (TaggedVal v1 _) == (TaggedVal v2 _) = v1 == v2

-- | Type of unique deterministic finite maps
--
-- The key is just here to keep us honest. It's always safe
-- to use a single type as key.
-- If two types don't overlap in their uniques it's also safe
-- to index the same map at multiple key types. But this is
-- very much discouraged.
data UniqDFM key ele =
  UDFM
    !(M.IntMap (TaggedVal ele)) -- A map where keys are Unique's values and
                                -- values are tagged with insertion time.
                                -- The invariant is that all the tags will
                                -- be distinct within a single map
    {-# UNPACK #-} !Int         -- Upper bound on the values' insertion
                                -- time. See Note [Overflow on plusUDFM]
  deriving (Data, Functor)

-- | Deterministic, in O(n log n).
instance Foldable (UniqDFM key) where
  foldr = foldUDFM

-- | Deterministic, in O(n log n).
instance Traversable (UniqDFM key) where
  traverse f = fmap listToUDFM_Directly
             . traverse (\(u,a) -> (u,) <$> f a)
             . udfmToList

emptyUDFM :: UniqDFM key elt
emptyUDFM = UDFM M.empty 0

unitUDFM :: Uniquable key => key -> elt -> UniqDFM key elt
unitUDFM k v = UDFM (M.singleton (getKey $ getUnique k) (TaggedVal v 0)) 1

-- The new binding always goes to the right of existing ones
addToUDFM :: Uniquable key => UniqDFM key elt -> key -> elt  -> UniqDFM key elt
addToUDFM m k v = addToUDFM_Directly m (getUnique k) v

-- The new binding always goes to the right of existing ones
addToUDFM_Directly :: UniqDFM key elt -> Unique -> elt -> UniqDFM key elt
addToUDFM_Directly (UDFM m i) u v
  = UDFM (MS.insertWith tf (getKey u) (TaggedVal v i) m) (i + 1)
  where
    tf (TaggedVal new_v _) (TaggedVal _ old_i) = TaggedVal new_v old_i
      -- Keep the old tag, but insert the new value
      -- This means that udfmToList typically returns elements
      -- in the order of insertion, rather than the reverse

      -- It is quite critical that the strict insertWith is used as otherwise
      -- the combination function 'tf' is not forced and both old values are retained
      -- in the map.

addToUDFM_C_Directly
  :: (elt -> elt -> elt)   -- old -> new -> result
  -> UniqDFM key elt
  -> Unique -> elt
  -> UniqDFM key elt
addToUDFM_C_Directly f (UDFM m i) u v
  = UDFM (MS.insertWith tf (getKey u) (TaggedVal v i) m) (i + 1)
    where
      tf (TaggedVal new_v _) (TaggedVal old_v old_i)
         = TaggedVal (f old_v new_v) old_i
          -- Flip the arguments, because M.insertWith uses  (new->old->result)
          --                         but f            needs (old->new->result)
          -- Like addToUDFM_Directly, keep the old tag

addToUDFM_C
  :: Uniquable key => (elt -> elt -> elt) -- old -> new -> result
  -> UniqDFM key elt -- old
  -> key -> elt -- new
  -> UniqDFM key elt -- result
addToUDFM_C f m k v = addToUDFM_C_Directly f m (getUnique k) v

addListToUDFM :: Uniquable key => UniqDFM key elt -> [(key,elt)] -> UniqDFM key elt
addListToUDFM = foldl' (\m (k, v) -> addToUDFM m k v)

addListToUDFM_Directly :: UniqDFM key elt -> [(Unique,elt)] -> UniqDFM key elt
addListToUDFM_Directly = foldl' (\m (k, v) -> addToUDFM_Directly m k v)

addListToUDFM_Directly_C
  :: (elt -> elt -> elt) -> UniqDFM key elt -> [(Unique,elt)] -> UniqDFM key elt
addListToUDFM_Directly_C f = foldl' (\m (k, v) -> addToUDFM_C_Directly f m k v)

delFromUDFM :: Uniquable key => UniqDFM key elt -> key -> UniqDFM key elt
delFromUDFM (UDFM m i) k = UDFM (M.delete (getKey $ getUnique k) m) i

plusUDFM_C :: (elt -> elt -> elt) -> UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
plusUDFM_C f udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft_C f udfml udfmr
  | otherwise = insertUDFMIntoLeft_C f udfmr udfml

-- Note [Overflow on plusUDFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- There are multiple ways of implementing plusUDFM.
-- The main problem that needs to be solved is overlap on times of
-- insertion between different keys in two maps.
-- Consider:
--
-- A = fromList [(a, (x, 1))]
-- B = fromList [(b, (y, 1))]
--
-- If you merge them naively you end up with:
--
-- C = fromList [(a, (x, 1)), (b, (y, 1))]
--
-- Which loses information about ordering and brings us back into
-- non-deterministic world.
--
-- The solution I considered before would increment the tags on one of the
-- sets by the upper bound of the other set. The problem with this approach
-- is that you'll run out of tags for some merge patterns.
-- Say you start with A with upper bound 1, you merge A with A to get A' and
-- the upper bound becomes 2. You merge A' with A' and the upper bound
-- doubles again. After 64 merges you overflow.
-- This solution would have the same time complexity as plusUFM, namely O(n+m).
--
-- The solution I ended up with has time complexity of
-- O(m log m + m * min (n+m, W)) where m is the smaller set.
-- It simply inserts the elements of the smaller set into the larger
-- set in the order that they were inserted into the smaller set. That's
-- O(m log m) for extracting the elements from the smaller set in the
-- insertion order and O(m * min(n+m, W)) to insert them into the bigger
-- set.

plusUDFM :: UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
plusUDFM udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft udfml udfmr
  | otherwise = insertUDFMIntoLeft udfmr udfml

insertUDFMIntoLeft :: UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
insertUDFMIntoLeft udfml udfmr = addListToUDFM_Directly udfml $ udfmToList udfmr

insertUDFMIntoLeft_C
  :: (elt -> elt -> elt) -> UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
insertUDFMIntoLeft_C f udfml udfmr =
  addListToUDFM_Directly_C f udfml $ udfmToList udfmr

lookupUDFM :: Uniquable key => UniqDFM key elt -> key -> Maybe elt
lookupUDFM (UDFM m _i) k = taggedFst `fmap` M.lookup (getKey $ getUnique k) m

lookupUDFM_Directly :: UniqDFM key elt -> Unique -> Maybe elt
lookupUDFM_Directly (UDFM m _i) k = taggedFst `fmap` M.lookup (getKey k) m

elemUDFM :: Uniquable key => key -> UniqDFM key elt -> Bool
elemUDFM k (UDFM m _i) = M.member (getKey $ getUnique k) m

-- | Performs a deterministic fold over the UniqDFM.
-- It's O(n log n) while the corresponding function on `UniqFM` is O(n).
foldUDFM :: (elt -> a -> a) -> a -> UniqDFM key elt -> a
foldUDFM k z m = foldr k z (eltsUDFM m)

-- | Performs a nondeterministic strict fold over the UniqDFM.
-- It's O(n), same as the corresponding function on `UniqFM`.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldUDFM :: (elt -> a -> a) -> a -> UniqDFM key elt -> a
nonDetStrictFoldUDFM k z (UDFM m _i) = foldl' k' z m
  where
    k' acc (TaggedVal v _) = k v acc

eltsUDFM :: UniqDFM key elt -> [elt]
eltsUDFM (UDFM m _i) =
  map taggedFst $ sortBy (compare `on` taggedSnd) $ M.elems m

filterUDFM :: (elt -> Bool) -> UniqDFM key elt -> UniqDFM key elt
filterUDFM p (UDFM m i) = UDFM (M.filter (\(TaggedVal v _) -> p v) m) i

filterUDFM_Directly :: (Unique -> elt -> Bool) -> UniqDFM key elt -> UniqDFM key elt
filterUDFM_Directly p (UDFM m i) = UDFM (M.filterWithKey p' m) i
  where
  p' k (TaggedVal v _) = p (getUnique k) v

-- | Converts `UniqDFM` to a list, with elements in deterministic order.
-- It's O(n log n) while the corresponding function on `UniqFM` is O(n).
udfmToList :: UniqDFM key elt -> [(Unique, elt)]
udfmToList (UDFM m _i) =
  [ (getUnique k, taggedFst v)
  | (k, v) <- sortBy (compare `on` (taggedSnd . snd)) $ M.toList m ]

-- Determines whether two 'UniqDFM's contain the same keys.
equalKeysUDFM :: UniqDFM key a -> UniqDFM key b -> Bool
equalKeysUDFM (UDFM m1 _) (UDFM m2 _) = liftEq (\_ _ -> True) m1 m2

isNullUDFM :: UniqDFM key elt -> Bool
isNullUDFM (UDFM m _) = M.null m

sizeUDFM :: UniqDFM key elt -> Int
sizeUDFM (UDFM m _i) = M.size m

intersectUDFM :: UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
intersectUDFM (UDFM x i) (UDFM y _j) = UDFM (M.intersection x y) i
  -- M.intersection is left biased, that means the result will only have
  -- a subset of elements from the left set, so `i` is a good upper bound.

udfmIntersectUFM :: UniqDFM key elt1 -> UniqFM key elt2 -> UniqDFM key elt1
udfmIntersectUFM (UDFM x i) y = UDFM (M.intersection x (ufmToIntMap y)) i
  -- M.intersection is left biased, that means the result will only have
  -- a subset of elements from the left set, so `i` is a good upper bound.

disjointUDFM :: UniqDFM key elt -> UniqDFM key elt -> Bool
disjointUDFM (UDFM x _i) (UDFM y _j) = M.disjoint x y

disjointUdfmUfm :: UniqDFM key elt -> UniqFM key elt2 -> Bool
disjointUdfmUfm (UDFM x _i) y = M.disjoint x (ufmToIntMap y)

minusUDFM :: UniqDFM key elt1 -> UniqDFM key elt2 -> UniqDFM key elt1
minusUDFM (UDFM x i) (UDFM y _j) = UDFM (M.difference x y) i
  -- M.difference returns a subset of a left set, so `i` is a good upper
  -- bound.

udfmMinusUFM :: UniqDFM key elt1 -> UniqFM key elt2 -> UniqDFM key elt1
udfmMinusUFM (UDFM x i) y = UDFM (M.difference x (ufmToIntMap y)) i
  -- M.difference returns a subset of a left set, so `i` is a good upper
  -- bound.

ufmMinusUDFM :: UniqFM key elt1 -> UniqDFM key elt2 -> UniqFM key elt1
ufmMinusUDFM x (UDFM y _i) = unsafeIntMapToUFM (M.difference (ufmToIntMap x) y)

-- | Partition UniqDFM into two UniqDFMs according to the predicate
partitionUDFM :: (elt -> Bool) -> UniqDFM key elt -> (UniqDFM key elt, UniqDFM key elt)
partitionUDFM p (UDFM m i) =
  case M.partition (p . taggedFst) m of
    (left, right) -> (UDFM left i, UDFM right i)

-- | Delete a list of elements from a UniqDFM
delListFromUDFM  :: Uniquable key => UniqDFM key elt -> [key] -> UniqDFM key elt
delListFromUDFM = foldl' delFromUDFM

-- | This allows for lossy conversion from UniqDFM to UniqFM
udfmToUfm :: UniqDFM key elt -> UniqFM key elt
udfmToUfm (UDFM m _i) = unsafeIntMapToUFM (M.map taggedFst m)

listToUDFM :: Uniquable key => [(key,elt)] -> UniqDFM key elt
listToUDFM = foldl' (\m (k, v) -> addToUDFM m k v) emptyUDFM

listToUDFM_Directly :: [(Unique, elt)] -> UniqDFM key elt
listToUDFM_Directly = foldl' (\m (u, v) -> addToUDFM_Directly m u v) emptyUDFM

-- | Apply a function to a particular element
adjustUDFM :: Uniquable key => (elt -> elt) -> UniqDFM key elt -> key -> UniqDFM key elt
adjustUDFM f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey $ getUnique k) m) i

-- | Apply a function to a particular element
adjustUDFM_Directly :: (elt -> elt) -> UniqDFM key elt -> Unique -> UniqDFM key elt
adjustUDFM_Directly f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey k) m) i

-- | The expression (alterUDFM f k map) alters value x at k, or absence
-- thereof. alterUDFM can be used to insert, delete, or update a value in
-- UniqDFM. Use addToUDFM, delFromUDFM or adjustUDFM when possible, they are
-- more efficient.
alterUDFM
  :: Uniquable key
  => (Maybe elt -> Maybe elt)  -- How to adjust
  -> UniqDFM key elt               -- old
  -> key                       -- new
  -> UniqDFM key elt               -- result
alterUDFM f (UDFM m i) k =
  UDFM (M.alter alterf (getKey $ getUnique k) m) (i + 1)
  where
  alterf Nothing = inject $ f Nothing
  alterf (Just (TaggedVal v _)) = inject $ f (Just v)
  inject Nothing = Nothing
  inject (Just v) = Just $ TaggedVal v i

-- | Map a function over every value in a UniqDFM
mapUDFM :: (elt1 -> elt2) -> UniqDFM key elt1 -> UniqDFM key elt2
mapUDFM f (UDFM m i) = UDFM (MS.map (fmap f) m) i
-- Critical this is strict map, otherwise you get a big space leak when reloading
-- in GHCi because all old ModDetails are retained (see pruneHomePackageTable).
-- Modify with care.

mapMaybeUDFM :: forall elt1 elt2 key.
                (elt1 -> Maybe elt2) -> UniqDFM key elt1 -> UniqDFM key elt2
mapMaybeUDFM f (UDFM m i) = UDFM (M.mapMaybe (traverse f) m) i

anyUDFM :: (elt -> Bool) -> UniqDFM key elt -> Bool
anyUDFM p (UDFM m _i) = M.foldr ((||) . p . taggedFst) False m

allUDFM :: (elt -> Bool) -> UniqDFM key elt -> Bool
allUDFM p (UDFM m _i) = M.foldr ((&&) . p . taggedFst) True m

instance Semi.Semigroup (UniqDFM key a) where
  (<>) = plusUDFM

instance Monoid (UniqDFM key a) where
  mempty = emptyUDFM
  mappend = (Semi.<>)

-- This should not be used in committed code, provided for convenience to
-- make ad-hoc conversions when developing
alwaysUnsafeUfmToUdfm :: UniqFM key elt -> UniqDFM key elt
alwaysUnsafeUfmToUdfm = listToUDFM_Directly . nonDetUFMToList

-- | Cast the key domain of a UniqFM.
--
-- As long as the domains don't overlap in their uniques
-- this is safe.
unsafeCastUDFMKey :: UniqDFM key1 elt -> UniqDFM key2 elt
unsafeCastUDFMKey = unsafeCoerce -- Only phantom parameter changes so
                                 -- this is safe and avoids reallocation.

-- Output-ery

instance Outputable a => Outputable (UniqDFM key a) where
    ppr ufm = pprUniqDFM ppr ufm

pprUniqDFM :: (a -> SDoc) -> UniqDFM key a -> SDoc
pprUniqDFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt elt
    | (uq, elt) <- udfmToList ufm ]

pprUDFM :: UniqDFM key a    -- ^ The things to be pretty printed
       -> ([a] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc          -- ^ 'SDoc' where the things have been pretty
                        -- printed
pprUDFM ufm pp = pp (eltsUDFM ufm)
