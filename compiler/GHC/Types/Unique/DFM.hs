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

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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
        upsertUDFM,
        alterUDFM,
        alterUDFM_L,
        mapUDFM,
        mapMaybeUDFM,
        mapMUDFM,
        plusUDFM,
        plusUDFM_C, plusUDFM_CK,
        lookupUDFM, lookupUDFM_Directly,
        elemUDFM,
        foldUDFM, foldWithKeyUDFM,
        eltsUDFM,
        filterUDFM, filterUDFM_Directly,
        isNullUDFM,
        sizeUDFM,
        intersectUDFM, udfmIntersectUFM,
        disjointUDFM, disjointUdfmUfm,
        equalKeysUDFM,
        minusUDFM,
        listToUDFM, listToUDFM_Directly,
        listToUDFM_C_Directly,
        udfmMinusUFM, ufmMinusUDFM,
        partitionUDFM,
        udfmRestrictKeys,
        udfmRestrictKeysSet,
        anyUDFM, allUDFM,
        pprUniqDFM, pprUDFM,

        udfmToList,
        udfmToUfm,
        nonDetStrictFoldUDFM,
        nonDetFoldUDFM,
        unsafeCastUDFMKey,
        alwaysUnsafeUfmToUdfm,
    ) where

import GHC.Prelude

import GHC.Types.Unique ( Uniquable(..), Unique, getKey, mkUniqueGrimily )
import GHC.Utils.Outputable

import qualified GHC.Data.Word64Map.Strict as MS
import qualified GHC.Data.Word64Map as M
import Data.Data
import Data.Functor.Classes (Eq1 (..))
import Data.List (sortBy)
import Data.Function (on)
import GHC.Types.Unique.FM (UniqFM, nonDetUFMToList, ufmToIntMap, unsafeIntMapToUFM)
import GHC.Data.SmallArray
import GHC.Exts (State#, build)
import GHC.ST (ST(..), runST)
import Unsafe.Coerce
import qualified GHC.Data.Word64Set as W

-- Note [Deterministic UniqFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A @UniqDFM@ is just like @UniqFM@ with the following additional
-- property: the function `udfmToList` returns the elements in some
-- deterministic order not depending on the Unique key for those elements.
--
-- If the client of the map performs operations on the map in deterministic
-- order then `udfmToList` returns them in deterministic order.
--
-- The order does not depend on how existing entries were
-- updated. Updating an existing entry keeps it original position in the order
-- This means `alterUDFM` consistent with `addToUDFM` and `adjustUDFM`,
-- so that for example `alterUDFM id k = id` and `alterUDFM (fmap f) k = adjustUDFM f k`
--
-- There is an implementation cost: each element is given an insertion tag
-- as it is added, and functions like `udfmToList` or `eltsUDFM` order their
-- results by this tag (see Note [Cost of deterministic iteration]). So you
-- should only use `UniqDFM` if you need the deterministic property.
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
-- every value with its insertion tag that can later be used to sort the
-- values when asked to convert to a list.
--
-- Updating an existing key keeps the old tag. This keeps the order stable for
-- maps whose entries are updated many times. The instance environments are
-- the main example: inserting an instance updates the entry of its class in a
-- DNameEnv, and when updates moved keys to the end the order of instances shown
-- by :info depended on the order in which interfaces happened to be loaded
-- (#27532). Now a class keeps its place once its first instance is added, so
-- loading further interfaces cannot change the order.
--
-- An alternative would be to have
--
--   data UniqDFM ele = UDFM (Word64Map ele) [ele]
--
-- where the list determines the order. This makes deletion tricky as we'd
-- only accumulate elements in that list, but makes merging easier as you
-- can just merge both structures independently.
-- Deletion can probably be done in amortized fashion when the size of the
-- list is twice the size of the set.

-- | A type of values carrying an insertion tag
data TaggedVal val =
  TaggedVal
    !val
    {-# UNPACK #-} !Int -- ^ insertion tag
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
    !(M.Word64Map (TaggedVal ele)) -- A map where keys are Unique's values and
                                   -- values carry an insertion tag.
    {-# UNPACK #-} !Int            -- Upper bound on the values' insertion
                                   -- tags. See Note [Overflow on plusUDFM]
  -- See Note [UDFM invariants]
  deriving (Data, Functor)

{- Note [UDFM invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
In a map (UDFM m ub):

 (a) The insertion tags of the elements of m are distinct.
 (b) Every tag lies in [0, ub).

Consequently ub >= size m.

The tags determine the order of deterministic iteration (eltsUDFM,
udfmToList). See Note [Sorting a UDFM].
-}

-- | Deterministic. See Note [Cost of deterministic iteration].
instance Foldable (UniqDFM key) where
  foldr = foldUDFM

-- | Deterministic. See Note [Cost of deterministic iteration].
instance Traversable (UniqDFM key) where
  traverse f = fmap listToUDFM_Directly
             . traverse (\(u,a) -> (u,) <$> f a)
             . udfmToList

emptyUDFM :: UniqDFM key elt
emptyUDFM = UDFM M.empty 0

unitUDFM :: Uniquable key => key -> elt -> UniqDFM key elt
unitUDFM k v = UDFM (M.singleton (getKey $ getUnique k) (TaggedVal v 0)) 1

-- A new key goes to the right of existing ones
-- Overwriting an existing key keeps its position in the iteration order
addToUDFM :: Uniquable key => UniqDFM key elt -> key -> elt  -> UniqDFM key elt
addToUDFM m k v = addToUDFM_Directly m (getUnique k) v

-- A new key goes to the right of existing ones
-- Overwriting an existing key keeps its position in the iteration order
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
{-# INLINEABLE addListToUDFM #-}

addListToUDFM_Directly :: UniqDFM key elt -> [(Unique,elt)] -> UniqDFM key elt
addListToUDFM_Directly = foldl' (\m (k, v) -> addToUDFM_Directly m k v)
{-# INLINEABLE addListToUDFM_Directly #-}

addListToUDFM_Directly_C
  :: (elt -> elt -> elt) -> UniqDFM key elt -> [(Unique,elt)] -> UniqDFM key elt
addListToUDFM_Directly_C f = foldl' (\m (k, v) -> addToUDFM_C_Directly f m k v)
{-# INLINEABLE addListToUDFM_Directly_C #-}

-- | Like 'addListToUDFM_Directly_C' but also passes the unique key to the combine function
addListToUDFM_Directly_CK
  :: (Unique -> elt -> elt -> elt) -> UniqDFM key elt -> [(Unique,elt)] -> UniqDFM key elt
addListToUDFM_Directly_CK f = foldl' (\m (k, v) -> addToUDFM_C_Directly (f k) m k v)
{-# INLINEABLE addListToUDFM_Directly_CK #-}

delFromUDFM :: Uniquable key => UniqDFM key elt -> key -> UniqDFM key elt
delFromUDFM (UDFM m i) k = UDFM (M.delete (getKey $ getUnique k) m) i

plusUDFM_C :: (elt -> elt -> elt) -> UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
plusUDFM_C f udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft_C f udfml udfmr
  | otherwise = insertUDFMIntoLeft_C f udfmr udfml

-- | Like 'plusUDFM_C' but the combine function also receives the unique key
plusUDFM_CK :: (Unique -> elt -> elt -> elt) -> UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
plusUDFM_CK f udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft_CK f udfml udfmr
  | otherwise = insertUDFMIntoLeft_CK f udfmr udfml


-- Note [Overflow on plusUDFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- There are multiple ways of implementing plusUDFM.
-- The main problem that needs to be solved is overlap on insertion
-- tags between different keys in two maps.
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

-- | Like 'insertUDFMIntoLeft_C', but the merge function also receives the unique key
insertUDFMIntoLeft_CK
  :: (Unique -> elt -> elt -> elt) -> UniqDFM key elt -> UniqDFM key elt -> UniqDFM key elt
insertUDFMIntoLeft_CK f udfml udfmr =
  addListToUDFM_Directly_CK f udfml $ udfmToList udfmr

lookupUDFM :: Uniquable key => UniqDFM key elt -> key -> Maybe elt
lookupUDFM (UDFM m _i) k = taggedFst `fmap` M.lookup (getKey $ getUnique k) m

lookupUDFM_Directly :: UniqDFM key elt -> Unique -> Maybe elt
lookupUDFM_Directly (UDFM m _i) k = taggedFst `fmap` M.lookup (getKey k) m

elemUDFM :: Uniquable key => key -> UniqDFM key elt -> Bool
elemUDFM k (UDFM m _i) = M.member (getKey $ getUnique k) m

-- | Performs a deterministic fold over the UniqDFM.
--
-- O(n) in the common case, with an O(n log n) fallback.
--
-- See Note [Cost of deterministic iteration].
foldUDFM :: (elt -> a -> a) -> a -> UniqDFM key elt -> a
{-# INLINE foldUDFM #-}
-- Specialises k and z into M.foldr on the small-map path.
foldUDFM k z (UDFM m ub)
  | M.compareSize m 1 /= GT = M.foldr (k . taggedFst) z m
  | otherwise               = fold_udfm k z m ub

fold_udfm :: (elt -> a -> a) -> a -> M.Word64Map (TaggedVal elt) -> Int -> a
{-# NOINLINE fold_udfm #-}
-- Kept out of line so that foldUDFM's consumers don't inline the sort machinery.
fold_udfm k z m ub
  | usePigeonholeSort m ub = foldr k z (pigeonholeSort ub (\_ tv -> tv) m)
  | otherwise              = foldr k z (map taggedFst (sort_it m))

-- | Like 'foldUDFM' but the function also receives a key.
--
-- See Note [Cost of deterministic iteration].
foldWithKeyUDFM :: (Unique -> elt -> a -> a) -> a -> UniqDFM key elt -> a
{-# INLINE foldWithKeyUDFM #-}
-- This INLINE was copied from foldUDFM
foldWithKeyUDFM k z m = foldr (uncurry k) z (udfmToList m)

-- | Performs a nondeterministic strict fold over the UniqDFM.
-- It's O(n), same as the corresponding function on `UniqFM`.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldUDFM :: (elt -> a -> a) -> a -> UniqDFM key elt -> a
nonDetStrictFoldUDFM k z (UDFM m _i) = foldl' k' z m
  where
    k' acc (TaggedVal v _) = k v acc

-- | Performs a nondeterministic lazy right fold over the UniqDFM.
-- It's O(n), and lazy in the accumulator, so unlike 'foldUDFM' it can
-- stream and short-circuit; see Note [Cost of deterministic iteration].
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUDFM :: (elt -> a -> a) -> a -> UniqDFM key elt -> a
{-# INLINE nonDetFoldUDFM #-}
nonDetFoldUDFM k z (UDFM m _i) = M.foldr (k . taggedFst) z m

{- Note [Cost of deterministic iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deterministic iteration -- foldUDFM, eltsUDFM, udfmToList, and everything
built on them -- orders elements by insertion tag. The element with the
smallest tag can sit anywhere in the map, so every tag must be inspected,
and, given a @UDFM m ub@ on the pigeonhole-sort path, an array with ub slots
must be filled, before the first element can be emitted (see
Note [Sorting a UDFM]). So beyond maps of a single element, deterministic
iteration cannot stream: demanding any of the result processes the whole
map. #27459 shows that cost hitting a consumer that only needed to know
whether the result was non-empty.

So: to test for emptiness, use isNullUDFM rather than null on eltsUDFM;
for order-oblivious queries, prefer short-circuiting anyUDFM/allUDFM; and
if you don't need the deterministic order at all, use nonDetStrictFoldUDFM
(or nonDetFoldUDFM when the fold should stream or short-circuit).
-}

-- | Deterministic, in order of insertion.
--
-- See Note [Sorting a UDFM] and Note [Cost of deterministic iteration].
eltsUDFM :: UniqDFM key elt -> [elt]
{-# INLINE eltsUDFM #-}  -- so the small case is a good producer
                         -- This matters for T13719.
eltsUDFM (UDFM m ub)
  | M.compareSize m 1 /= GT = build (\c n -> M.foldr (c . taggedFst) n m)
  | otherwise               = elts_udfm m ub

elts_udfm :: M.Word64Map (TaggedVal elt) -> Int -> [elt]
{-# NOINLINE elts_udfm #-}
-- Kept out of line so that eltsUDFM's consumers don't inline the sort machinery.
elts_udfm m ub
  | usePigeonholeSort m ub = pigeonholeSort ub (\_ tv -> tv) m
  | otherwise              = map taggedFst (sort_it m)

sort_it :: M.Word64Map (TaggedVal elt) -> [TaggedVal elt]
sort_it m = sortBy (compare `on` taggedSnd) (M.elems m)


{- Note [Sorting a UDFM]
~~~~~~~~~~~~~~~~~~~~~~~~
Deterministic iteration must yield a map's elements in order of their
insertion tags. The obvious way is to sort on the tags, but we can do better:
in (UDFM m ub) the tags are distinct indices into [0, ub) (see
Note [UDFM invariants]), so each element can simply be placed at its own
tag in an ub-slot array, which is then read back in index order. This is
pigeonhole sort, with one element per hole.

Cost: writing the elements is O(n) for n = M.size m, while allocating the
array and reading it back are O(ub). Since n <= ub the total is O(ub). No
comparisons are made.

So the method wins only while the array stays dense, and ub never shrinks
(overwrites keep bumping it, delete/filter shrink n but not ub).
usePigeonholeSort therefore takes this path only when ub <= 4 * n, which
bounds its cost at O(n), and falls back to the O(n log n) comparison sort
otherwise.

Unfilled slots contain a TaggedVal with tag -1 and value
@unsafeCoerce () :: r@. This is safe because the value is never used: only
slots with non-negative tags are read.

pigeonholeSort also avoids intermediate lists: it fills the array by
traversing the map directly, and emits its readout with 'build', so the foldr
in fold_udfm fuses with it. This contributes significantly to the allocation
reductions in InstanceMatching1 in !16292.
-}

-- | @ub <= 4 * size m@, computed without a full 'M.size' traversal.
usePigeonholeSort :: M.Word64Map a -> Int -> Bool
usePigeonholeSort m ub = M.compareSize m ceil_ub_div_4 /= LT
  where
    ceil_ub_div_4 = (ub + 3) `div` 4  -- ceil(ub/4): ub <= 4*n iff n >= ceil(ub/4)

-- | Order the map's elements by tag. The tags must be distinct and in
-- @[0, ub)@, and @mk@ must preserve them. See Note [Sorting a UDFM].
pigeonholeSort :: forall e r. Int
              -> (M.Key -> TaggedVal e -> TaggedVal r)
              -> M.Word64Map (TaggedVal e)
              -> [r]
{-# INLINE pigeonholeSort #-}  -- Specialise mk and enable foldr/build fusion.
pigeonholeSort ub mk m = build gen
  where
    -- The tag -1 marks unfilled slots; the value field is never read, but it
    -- is strict, so it needs a WHNF value of type r. See Note [Sorting a UDFM].
    hole :: TaggedVal r
    hole = TaggedVal (unsafeCoerce ()) (-1)

    fill :: SmallMutableArray s (TaggedVal r) -> State# s -> (# State# s, () #)
    fill marr s = case M.traverseWithKey_ write m of ST st -> st s
      where
        write k tv = ST (\s' ->
          (# writeSmallArray marr (taggedSnd tv) (mk k tv) s', () #))

    gen :: forall b. (r -> b -> b) -> b -> b
    gen cons nil = runST (ST (\s0 ->
      case newSmallArray ub hole s0 of
        (# s1, marr #) -> case fill marr s1 of
          (# s2, () #) -> case unsafeFreezeSmallArray marr s2 of
            (# s3, arr #) -> (# s3, readout arr 0 #)))
      where
        readout :: SmallArray (TaggedVal r) -> Int -> b
        readout arr j
          | j >= ub   = nil
          | t < 0     = readout arr (j + 1)
          | otherwise = cons v (readout arr (j + 1))
          where TaggedVal v t = indexSmallArray arr j

filterUDFM :: (elt -> Bool) -> UniqDFM key elt -> UniqDFM key elt
filterUDFM p (UDFM m i) = UDFM (M.filter (\(TaggedVal v _) -> p v) m) i

filterUDFM_Directly :: (Unique -> elt -> Bool) -> UniqDFM key elt -> UniqDFM key elt
filterUDFM_Directly p (UDFM m i) = UDFM (M.filterWithKey p' m) i
  where
  p' k (TaggedVal v _) = p (mkUniqueGrimily k) v

udfmRestrictKeys :: UniqDFM key elt -> UniqDFM key elt2 -> UniqDFM key elt
udfmRestrictKeys (UDFM a i) (UDFM b _) = UDFM (M.restrictKeys a (M.keysSet b)) i

udfmRestrictKeysSet :: UniqDFM key elt -> W.Word64Set -> UniqDFM key elt
udfmRestrictKeysSet (UDFM val_set i) set =
  let key_set = set
  in UDFM (M.restrictKeys val_set key_set) i

-- | Converts `UniqDFM` to a list, with elements in deterministic order.
--
-- O(n) in the common case, with an O(n log n) fallback.
--
-- See Note [Cost of deterministic iteration].
udfmToList :: UniqDFM key elt -> [(Unique, elt)]
-- NB: no INLINE, unlike eltsUDFM. udfmToList's one hot consumer is
-- traverseUSDFM in the pattern-match checker, which doesn't fuse. Inlining
-- the size dispatch into it regresses T17836.
udfmToList (UDFM m ub)
  | M.compareSize m 1 /= GT =
      M.foldrWithKey (\k tv r -> (mkUniqueGrimily k, taggedFst tv) : r) [] m
  | usePigeonholeSort m ub = pigeonholeSort ub
      (\k tv -> TaggedVal (mkUniqueGrimily k, taggedFst tv) (taggedSnd tv)) m
  | otherwise =
      [ (mkUniqueGrimily k, taggedFst v)
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

listToUDFM_C_Directly :: (elt -> elt -> elt) -> [(Unique, elt)] -> UniqDFM key elt
listToUDFM_C_Directly f = foldl' (\m (u, v) -> addToUDFM_C_Directly f m u v) emptyUDFM

-- | Apply a function to a particular element
adjustUDFM :: Uniquable key => (elt -> elt) -> UniqDFM key elt -> key -> UniqDFM key elt
adjustUDFM f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey $ getUnique k) m) i

-- | Apply a function to a particular element
adjustUDFM_Directly :: (elt -> elt) -> UniqDFM key elt -> Unique -> UniqDFM key elt
adjustUDFM_Directly f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey k) m) i

-- | The expression (@'alterUDFM' f map k@) alters value x at k, or absence
-- thereof. 'alterUDFM' can be used to insert, delete, or update a value in
-- UniqDFM. Use addToUDFM, delFromUDFM or adjustUDFM when possible, they are
-- more efficient. Updating an existing key keeps its position in the
-- deterministic iteration order.
--
-- 'alterUDFM' is non-strict in @k@.
alterUDFM
  :: Uniquable key
  => (Maybe elt -> Maybe elt)  -- ^ How to adjust the element
  -> UniqDFM key elt           -- ^ Old 'UniqDFM'
  -> key                       -- ^ @key@ of the element to adjust
  -> UniqDFM key elt           -- ^ New element at @key@ and modified 'UniqDFM'
alterUDFM f (UDFM m i) k =
  UDFM (M.alter alterf (getKey $ getUnique k) m) (i + 1)
  where
  alterf Nothing = inject i $ f Nothing
  alterf (Just (TaggedVal v old_i)) = inject old_i $ f (Just v)
  inject _ Nothing = Nothing
  inject tag (Just v) = Just $ TaggedVal v tag

-- | The expression (@'upsertUDFM' f map k@) updates the value at @k@ or inserts
-- a new value if @k@ is absent.
--
-- Updating an existing entry keeps its original tag, so its position in
-- deterministic iteration order is unchanged and does not depend on update order.
upsertUDFM
  :: Uniquable key
  => (Maybe elt -> elt)  -- ^ How to adjust the element
  -> UniqDFM key elt     -- ^ Old 'UniqDFM'
  -> key                 -- ^ @key@ of the element to adjust
  -> UniqDFM key elt     -- ^ New element at @key@ and modified 'UniqDFM'
upsertUDFM f (UDFM m i) k =
  UDFM (MS.upsert upsertf (getKey $ getUnique k) m) (i + 1)
  where
    upsertf Nothing = TaggedVal (f Nothing) i
    upsertf (Just (TaggedVal v old_i)) = TaggedVal (f (Just v)) old_i

-- | The expression (@'alterUDFM_L' f map k@) alters value @x@ at @k@, or absence
-- thereof and returns the new element at @k@ if there is any.
-- 'alterUDFM_L' can be used to insert, delete, or update a value in
-- UniqDFM. Use addToUDFM, delFromUDFM or adjustUDFM when possible, they are
-- more efficient. Updating an existing key keeps its position in the
-- deterministic iteration order.
--
-- Note, 'alterUDFM_L' is strict in @k@.
alterUDFM_L
  :: forall key elt . Uniquable key
  => (Maybe elt -> Maybe elt)      -- ^ How to adjust the element
  -> UniqDFM key elt               -- ^ Old 'UniqDFM'
  -> key                           -- ^ @key@ of the element to adjust
  -> (Maybe elt, UniqDFM key elt)  -- ^ New element at @key@ and modified 'UniqDFM'
alterUDFM_L f (UDFM m i) k =
  let
    (mElt, udfm) = M.alterLookup alterf (getKey $ getUnique k) m
  in
    (fmap taggedFst mElt, UDFM udfm (i + 1))
  where
  alterf :: Maybe (TaggedVal elt) -> (Maybe (TaggedVal elt))
  alterf Nothing = inject i $ f Nothing
  alterf (Just (TaggedVal v old_i)) = inject old_i $ f (Just v)
  inject _ Nothing = Nothing
  inject tag (Just v) = Just $ TaggedVal v tag

-- | Map a function over every value in a UniqDFM
mapUDFM :: (elt1 -> elt2) -> UniqDFM key elt1 -> UniqDFM key elt2
mapUDFM f (UDFM m i) = UDFM (MS.map (fmap f) m) i
-- Critical this is strict map, otherwise you get a big space leak when reloading
-- in GHCi because all old ModDetails are retained (see pruneHomePackageTable).
-- Modify with care.

{-# INLINEABLE mapMUDFM #-}
-- | 'mapM' for a 'UniqDFM'.
mapMUDFM :: Monad m => (elt1 -> m elt2) -> UniqDFM key elt1 -> m (UniqDFM key elt2)
mapMUDFM f (UDFM m i) = do
  m' <- traverse (traverse f) m
  return $ UDFM m' i

mapMaybeUDFM :: forall elt1 elt2 key.
                (elt1 -> Maybe elt2) -> UniqDFM key elt1 -> UniqDFM key elt2
mapMaybeUDFM f (UDFM m i) = UDFM (M.mapMaybe (traverse f) m) i

anyUDFM :: (elt -> Bool) -> UniqDFM key elt -> Bool
anyUDFM p (UDFM m _i) = M.foldr ((||) . p . taggedFst) False m

allUDFM :: (elt -> Bool) -> UniqDFM key elt -> Bool
allUDFM p (UDFM m _i) = M.foldr ((&&) . p . taggedFst) True m

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
