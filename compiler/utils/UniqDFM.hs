{-
(c) Bartosz Nitka, Facebook, 2015

UniqDFM: Specialised deterministic finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

This is very similar to @UniqFM@, the major difference being that the order of
folding is not dependent on @Unique@ ordering, giving determinism.
Currently the ordering is determined by insertion order.

See Note [Unique Determinism] in Unique for explanation why @Unique@ ordering
is not deterministic.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module UniqDFM (
        -- * Unique-keyed deterministic mappings
        UniqDFM,       -- abstract type

        -- ** Manipulating those mappings
        emptyUDFM,
        addToUDFM,
        lookupUDFM,
        foldUDFM,
        eltsUDFM,
        udfmToList,
    ) where

import FastString
import Unique           ( Uniquable(..), Unique, getKey )
import Outputable

import qualified Data.IntMap as M
import Data.Typeable
import Data.Data
import Data.List (sortBy)
import Data.Function (on)

-- Note [Deterministic UniqFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Normal @UniqFM@ when you turn it into a list will use
-- Data.IntMap.toList function that returns the elements in the order of
-- the keys. The keys in @UniqFM@ are always @Uniques@, so you end up with
-- with a list ordered by @Uniques@.
-- The order of @Uniques@ is known to be not stable across rebuilds.
-- See Note [Unique Determinism] in Unique.

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
-- don't have to renumber everything.
-- I've tested both approaches by replacing UniqFM and the cost was about
-- the same for both. We don't need merging nor deletion yet, but when we
-- do it might be worth to reevaluate the trade-offs here.

data TaggedVal val = TaggedVal val {-# UNPACK #-} !Int
  deriving (Data, Typeable)

taggedFst :: TaggedVal val -> val
taggedFst (TaggedVal v _) = v

taggedSnd :: TaggedVal val -> Int
taggedSnd (TaggedVal _ i) = i

instance Eq val => Eq (TaggedVal val) where
  (TaggedVal v1 _) == (TaggedVal v2 _) = v1 == v2

instance Functor TaggedVal where
  fmap f (TaggedVal val i) = TaggedVal (f val) i

data UniqDFM ele = UDFM !(M.IntMap (TaggedVal ele)) {-# UNPACK #-} !Int
  deriving (Data, Typeable, Functor)

emptyUDFM :: UniqDFM elt
emptyUDFM = UDFM M.empty 0

addToUDFM :: Uniquable key => UniqDFM elt -> key -> elt  -> UniqDFM elt
addToUDFM (UDFM m i) k v =
  UDFM (M.insert (getKey $ getUnique k) (TaggedVal v i) m) (i + 1)

lookupUDFM :: Uniquable key => UniqDFM elt -> key -> Maybe elt
lookupUDFM (UDFM m _i) k = taggedFst `fmap` M.lookup (getKey $ getUnique k) m

foldUDFM :: (elt -> a -> a) -> a -> UniqDFM elt -> a
foldUDFM k z m = foldr k z (eltsUDFM m)

eltsUDFM :: UniqDFM elt -> [elt]
eltsUDFM (UDFM m _i) =
  map taggedFst $ sortBy (compare `on` taggedSnd) $ M.elems m

udfmToList :: UniqDFM elt -> [(Unique, elt)]
udfmToList (UDFM m _i) =
  [ (getUnique k, taggedFst v)
  | (k, v) <- sortBy (compare `on` (taggedSnd . snd)) $ M.toList m ]

-- Output-ery

instance Outputable a => Outputable (UniqDFM a) where
    ppr ufm = pprUniqDFM ppr ufm

pprUniqDFM :: (a -> SDoc) -> UniqDFM a -> SDoc
pprUniqDFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> ptext (sLit ":->") <+> ppr_elt elt
    | (uq, elt) <- udfmToList ufm ]
