{-# LANGUAGE QuantifiedConstraints #-}

-- | Dependent sum types and dependent maps.
--
-- A minimal re-implementation of what GHC needs from @dependent-sum@ and
-- @dependent-map@, to avoid incurring a dependency on those libraries.
--
-- The implementation is less efficient: by using @Map (Some k) (DSum k f)@
-- instead of a native dependent map (a balanced tree storing @k a@ and @f a@
-- directly in each node), this costs:
--
--   * allocating one 'Some' and one 'DSum' constructor application per node
--     (the key itself is shared),
--
--   * one extra 'geq' comparison per successful lookup.
module GHC.Data.Dependent
  ( -- * Operations on keys
    GEq(..)
  , GCompare(..)
  , GOrdering(..)
  , gcompareOrd
  , (:~:)(..)

    -- * Dependent sums
  , Some(..)
  , DSum(..)

    -- * Dependent maps
  , DMap
  , emptyDMap
  , lookupDMap
  , memberDMap
  , insertNewDMap
  , toListDMap
  , fromListDMap
  , traverseMaybeWithKeyDMap
  ) where

import GHC.Prelude

import GHC.Utils.Outputable ( Outputable(..) )
import GHC.Utils.Panic ( panic )

import Data.Type.Equality ( (:~:)(..) )
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- * Keys
--------------------------------------------------------------------------------

class GEq k where
  geq :: k a -> k b -> Maybe (a :~: b)

data GOrdering a b where
  GLT :: GOrdering a b
  GEQ :: GOrdering a a
  GGT :: GOrdering a b

class GEq k => GCompare k where
  gcompare :: k a -> k b -> GOrdering a b

gcompareOrd :: Ord x => x -> x -> GOrdering a a
gcompareOrd x y =
  case compare x y of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT
{-# INLINE gcompareOrd #-}

--------------------------------------------------------------------------------
-- * Existentials
--------------------------------------------------------------------------------

data Some k where
  Some :: !(k a) -> Some k

instance GEq k => Eq (Some k) where
  Some x == Some y = isJustRefl (geq x y)
    where
      isJustRefl :: Maybe (a :~: b) -> Bool
      isJustRefl Nothing = False
      isJustRefl (Just Refl) = True

instance GCompare k => Ord (Some k) where
  compare (Some x) (Some y) = case gcompare x y of
    GLT -> LT
    GEQ -> EQ
    GGT -> GT

instance (forall x. Outputable (k x)) => Outputable (Some k) where
  ppr (Some q) = ppr q

data DSum k f where
  (:=>) :: !(k a) -> !(f a) -> DSum k f

infixr 1 :=>

dsumKey :: DSum k f -> Some k
dsumKey (k :=> _) = Some k

--------------------------------------------------------------------------------
-- * Dependent maps
--------------------------------------------------------------------------------

newtype DMap k f = DMap (Map.Map (Some k) (DSum k f))

emptyDMap :: DMap k f
emptyDMap = DMap Map.empty

matchKey :: GEq k => k a -> DSum k f -> f a
matchKey k (k' :=> v) = case geq k k' of
  Just Refl -> v
  Nothing   -> panic "GHC.Data.Dependent: inconsistent GEq/GCompare instance"

lookupDMap :: GCompare k => k a -> DMap k f -> Maybe (f a)
lookupDMap k (DMap m) = case Map.lookup (Some k) m of
  Nothing -> Nothing
  Just e  -> Just $! matchKey k e
{-# INLINABLE lookupDMap #-}

memberDMap :: GCompare k => k a -> DMap k f -> Bool
memberDMap k (DMap m) = Map.member (Some k) m
{-# INLINABLE memberDMap #-}

-- | Insert an entry if the key is absent, returning whether it was inserted.
--
-- If the key was already present, the map is returned unchanged.
insertNewDMap :: GCompare k => k a -> f a -> DMap k f -> (DMap k f, Bool)
insertNewDMap k v (DMap m) =
  case Map.alterF claim (Some k) m of
    (True , m') -> (DMap m', True)
    (False, _ ) -> (DMap m , False)
  where
    claim Nothing    = (True , Just (k :=> v))
    claim (Just old) = (False, Just old)
{-# INLINABLE insertNewDMap #-}

-- | Returns all entries of a dependent map, in ascending key order.
toListDMap :: DMap k f -> [DSum k f]
toListDMap (DMap m) = Map.elems m

fromListDMap :: GCompare k => [DSum k f] -> DMap k f
fromListDMap entries = DMap (Map.fromList [ (dsumKey e, e) | e <- entries ])
{-# INLINABLE fromListDMap #-}

traverseMaybeWithKeyDMap
  :: Applicative m
  => (forall a. k a -> f a -> m (Maybe (g a))) -> DMap k f -> m (DMap k g)
traverseMaybeWithKeyDMap f (DMap m) = DMap <$> Map.traverseMaybeWithKey entry m
  where
    entry _ (k :=> v) = fmap (k :=>) <$> f k v
