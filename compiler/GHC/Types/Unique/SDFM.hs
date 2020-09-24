{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wall #-}

-- | Like a 'UniqDFM', but maintains equivalence classes of keys sharing the
-- same entry. See 'UniqSDFM'.
module GHC.Types.Unique.SDFM (
        -- * Unique-keyed, /shared/, deterministic mappings
        UniqSDFM,

        emptyUSDFM,
        lookupUSDFM, lookupReprAndEntryUSDFM, sameReprUSDFM,
        mergeEntriesUSDFM, addToUSDFM,
        traverseUSDFM
    ) where

import GHC.Prelude

import GHC.Types.Unique
import GHC.Types.Unique.DFM
import GHC.Utils.Outputable

import Data.Function (on)

-- | Either @Indirect x@, meaning the value is represented by that of @x@, or
-- an @Entry@ containing containing the actual value it represents.
data Shared key ele
  = Indirect !key
  | Entry !ele

-- | A 'UniqDFM' whose domain is /sets/ of 'Unique's, each of which share a
-- common value of type @ele@.
-- Every such set (\"equivalence class\") has a distinct representative
-- 'Unique'. Supports merging the entries of multiple such sets in a union-find
-- like fashion.
--
-- An accurate model is that of a pair of sets of uniques and an assocation list
-- on the representatives 'Unique's of those sets as keys, e.g.
-- @m = ([{u1,u3},{u2},{u4,u7}], [(u1, ele1), (u2, ele2), (u4, ele3)])@, where
-- the first element of each set is its representative. On this model we support
-- the following main operations:
--
--   * @'lookupUSDFM' m u3 == Just ele1@
--   * @'lookupReprAndEntryUSDFM' m u3 == (u1, ele1)@ because @u1@ is the
--     representative of @u3@'s set.
--   * @'sameReprUDFM' m u1 u3 == True@, but @'sameReprUDFM' u1 u2 == False@.
--   * @'mergeEntriesUDFM' m u1 u3@ is a no-op, but
--     @'mergeEntriesUDFM' m u1 u2@ adds @{u1,u3}@ to the set of @u2@ and
--     returns the old entry of @{u1,u3}@.
--   * @'addToUSDFM' m u3 ele4@ sets the entry of @{u1,u3}@ to @ele4@.
--
-- As well as a few means for traversal/conversion to list.
newtype UniqSDFM key ele
  = USDFM { unUSDFM :: UniqDFM key (Shared key ele) }

emptyUSDFM :: UniqSDFM key ele
emptyUSDFM = USDFM emptyUDFM

lookupReprAndEntryUSDFM :: Uniquable key => UniqSDFM key ele -> key -> (key, Maybe ele)
lookupReprAndEntryUSDFM (USDFM env) = go
  where
    go x = case lookupUDFM env x of
      Nothing           -> (x, Nothing)
      Just (Indirect y) -> go y
      Just (Entry ele)  -> (x, Just ele)

-- | @lookupSUDFM env x@ looks up an entry for @x@, looking through all
-- 'Indirect's until it finds a shared 'Entry'.
--
-- Examples in terms of the model (see 'UniqSDFM'):
-- >>> lookupUSDFM ([{u1,u3},{u2}], [(u1, ele1), (u2, ele2)]) u3 == Just ele1
-- >>> lookupUSDFM ([{u1,u3},{u2}], [(u1, ele1), (u2, ele2)]) u4 == Nothing
lookupUSDFM :: Uniquable key => UniqSDFM key ele -> key -> Maybe ele
lookupUSDFM usdfm x = snd (lookupReprAndEntryUSDFM usdfm x)

-- | Check if two variables are part of the same equivalence class.
--
-- Examples in terms of the model (see 'UniqSDFM'):
-- >>> sameReprUSDFM ([{u1,u3},{u2}], [(u1, ele1), (u2, ele2)]) u1 u3 == True
-- >>> sameReprUSDFM ([{u1,u3},{u2}], [(u1, ele1), (u2, ele2)]) u1 u2 == False
-- >>> sameReprUSDFM ([{u1,u3},{u2}], []) u1 u3 == True
sameReprUSDFM :: Uniquable key => UniqSDFM key ele -> key -> key -> Bool
sameReprUSDFM usdfm = (==) `on` getUnique . fst . lookupReprAndEntryUSDFM usdfm

-- | @mergeEntriesUSDFM env x y@ makes @x@ and @y@ point to the same entry,
-- thereby merging @x@'s class with @y@'s.
-- If both @x@ and @y@ are in the domain of the map, then @y@'s entry will be
-- chosen as the new entry and @x@'s old entry will be returned.
--
-- Examples in terms of the model (see 'UniqSDFM'):
-- >>> mergeEntriesUSDFM ([], []) u1 u2 == (Nothing, ([{u1,u2}], []))
-- >>> mergeEntriesUSDFM ([{u1,u3}], [(u1, ele1)]) u3 u4 == (Nothing, ([{u1,u3,u4}], [(u1, ele1)]))
-- >>> mergeEntriesUSDFM ([{u1,u3}], [(u1, ele1)]) u4 u3 == (Nothing, ([{u1,u3,u4}], [(u1, ele1)]))
-- >>> mergeEntriesUSDFM ([{u1,u3},{u2}], [(u1, ele1), (u2, ele2)]) u3 u2 == (Just ele1, ([{u2,u1,u3}], [(u2, ele2)]))
mergeEntriesUSDFM
  :: Uniquable key => UniqSDFM key ele -> key -> key -> (Maybe ele, UniqSDFM key ele)
mergeEntriesUSDFM usdfm@(USDFM env) x y =
  case (lu x, lu y) of
    ((x', _)   , (_, Nothing)) -> (Nothing, set_indirect y x')
    ((_, mb_ex), (y', _))      -> (mb_ex,   set_indirect x y')
  where
    lu = lookupReprAndEntryUSDFM usdfm
    set_indirect a b = USDFM $ addToUDFM env a (Indirect b)


-- | @addToUSDFM env x a@ sets the 'Entry' @x@ is associated with to @a@,
-- thereby modifying its whole equivalence class.
--
-- Examples in terms of the model (see 'UniqSDFM'):
-- >>> addToUSDFM ([], []) u1 ele1 == ([{u1}], [(u1, ele1)])
-- >>> addToUSDFM ([{u1,u3}], [(u1, ele1)]) u3 ele2 == ([{u1,u3}], [(u1, ele2)])
addToUSDFM :: Uniquable key => UniqSDFM key ele -> key -> ele -> UniqSDFM key ele
addToUSDFM usdfm@(USDFM env) x v =
  USDFM $ addToUDFM env (fst (lookupReprAndEntryUSDFM usdfm x)) (Entry v)

traverseUSDFM :: forall key a b f. Applicative f => (a -> f b) -> UniqSDFM key a -> f (UniqSDFM key b)
traverseUSDFM f = fmap (USDFM . listToUDFM_Directly) . traverse g . udfmToList . unUSDFM
  where
    g :: (Unique, Shared key a) -> f (Unique, Shared key b)
    g (u, Indirect y) = pure (u,Indirect y)
    g (u, Entry a)    = do
        a' <- f a
        pure (u,Entry a')

instance (Outputable key, Outputable ele) => Outputable (Shared key ele) where
  ppr (Indirect x) = ppr x
  ppr (Entry a)    = ppr a

instance (Outputable key, Outputable ele) => Outputable (UniqSDFM key ele) where
  ppr (USDFM env) = ppr env
