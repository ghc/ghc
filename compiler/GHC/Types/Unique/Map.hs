{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

-- Like 'UniqFM', these are maps for keys which are Uniquable.
-- Unlike 'UniqFM', these maps also remember their keys, which
-- makes them a much better drop in replacement for 'Data.Map.Map'.
--
-- Key preservation is right-biased.
module GHC.Types.Unique.Map (
    UniqMap(..),
    emptyUniqMap,
    isNullUniqMap,
    unitUniqMap,
    listToUniqMap,
    listToUniqMap_C,
    addToUniqMap,
    addListToUniqMap,
    addToUniqMap_C,
    addToUniqMap_Acc,
    alterUniqMap,
    addListToUniqMap_C,
    adjustUniqMap,
    delFromUniqMap,
    delListFromUniqMap,
    plusUniqMap,
    plusUniqMap_C,
    plusMaybeUniqMap_C,
    plusUniqMapList,
    minusUniqMap,
    intersectUniqMap,
    disjointUniqMap,
    mapUniqMap,
    filterUniqMap,
    partitionUniqMap,
    sizeUniqMap,
    elemUniqMap,
    lookupUniqMap,
    lookupWithDefaultUniqMap,
    anyUniqMap,
    allUniqMap,
    nonDetEltsUniqMap
    -- Non-deterministic functions omitted
) where

import GHC.Prelude

import GHC.Types.Unique.FM

import GHC.Types.Unique
import GHC.Utils.Outputable

import Data.Semigroup as Semi ( Semigroup(..) )
import Data.Coerce
import Data.Maybe
import Data.Data

-- | Maps indexed by 'Uniquable' keys
newtype UniqMap k a = UniqMap (UniqFM k (k, a))
    deriving (Data, Eq, Functor)
type role UniqMap nominal representational

instance Semigroup (UniqMap k a) where
  (<>) = plusUniqMap

instance Monoid (UniqMap k a) where
    mempty = emptyUniqMap
    mappend = (Semi.<>)

instance (Outputable k, Outputable a) => Outputable (UniqMap k a) where
    ppr (UniqMap m) =
        brackets $ fsep $ punctuate comma $
        [ ppr k <+> text "->" <+> ppr v
        | (k, v) <- nonDetEltsUFM m ]

liftC :: (a -> a -> a) -> (k, a) -> (k, a) -> (k, a)
liftC f (_, v) (k', v') = (k', f v v')

emptyUniqMap :: UniqMap k a
emptyUniqMap = UniqMap emptyUFM

isNullUniqMap :: UniqMap k a -> Bool
isNullUniqMap (UniqMap m) = isNullUFM m

unitUniqMap :: Uniquable k => k -> a -> UniqMap k a
unitUniqMap k v = UniqMap (unitUFM k (k, v))

listToUniqMap :: Uniquable k => [(k,a)] -> UniqMap k a
listToUniqMap kvs = UniqMap (listToUFM [ (k,(k,v)) | (k,v) <- kvs])

listToUniqMap_C :: Uniquable k => (a -> a -> a) -> [(k,a)] -> UniqMap k a
listToUniqMap_C f kvs = UniqMap $
    listToUFM_C (liftC f) [ (k,(k,v)) | (k,v) <- kvs]

addToUniqMap :: Uniquable k => UniqMap k a -> k -> a -> UniqMap k a
addToUniqMap (UniqMap m) k v = UniqMap $ addToUFM m k (k, v)

addListToUniqMap :: Uniquable k => UniqMap k a -> [(k,a)] -> UniqMap k a
addListToUniqMap (UniqMap m) kvs = UniqMap $
    addListToUFM m [(k,(k,v)) | (k,v) <- kvs]

addToUniqMap_C :: Uniquable k
               => (a -> a -> a)
               -> UniqMap k a
               -> k
               -> a
               -> UniqMap k a
addToUniqMap_C f (UniqMap m) k v = UniqMap $
    addToUFM_C (liftC f) m k (k, v)

addToUniqMap_Acc :: Uniquable k
                 => (b -> a -> a)
                 -> (b -> a)
                 -> UniqMap k a
                 -> k
                 -> b
                 -> UniqMap k a
addToUniqMap_Acc exi new (UniqMap m) k0 v0 = UniqMap $
    addToUFM_Acc (\b (k, v) -> (k, exi b v))
                 (\b -> (k0, new b))
                 m k0 v0

alterUniqMap :: Uniquable k
             => (Maybe a -> Maybe a)
             -> UniqMap k a
             -> k
             -> UniqMap k a
alterUniqMap f (UniqMap m) k = UniqMap $
    alterUFM (fmap (k,) . f . fmap snd) m k

addListToUniqMap_C
    :: Uniquable k
    => (a -> a -> a)
    -> UniqMap k a
    -> [(k, a)]
    -> UniqMap k a
addListToUniqMap_C f (UniqMap m) kvs = UniqMap $
    addListToUFM_C (liftC f) m
        [(k,(k,v)) | (k,v) <- kvs]

adjustUniqMap
    :: Uniquable k
    => (a -> a)
    -> UniqMap k a
    -> k
    -> UniqMap k a
adjustUniqMap f (UniqMap m) k = UniqMap $
    adjustUFM (\(_,v) -> (k,f v)) m k

delFromUniqMap :: Uniquable k => UniqMap k a -> k -> UniqMap k a
delFromUniqMap (UniqMap m) k = UniqMap $ delFromUFM m k

delListFromUniqMap :: Uniquable k => UniqMap k a -> [k] -> UniqMap k a
delListFromUniqMap (UniqMap m) ks = UniqMap $ delListFromUFM m ks

plusUniqMap :: UniqMap k a -> UniqMap k a -> UniqMap k a
plusUniqMap (UniqMap m1) (UniqMap m2) = UniqMap $ plusUFM m1 m2

plusUniqMap_C :: (a -> a -> a) -> UniqMap k a -> UniqMap k a -> UniqMap k a
plusUniqMap_C f (UniqMap m1) (UniqMap m2) = UniqMap $
    plusUFM_C (liftC f) m1 m2

plusMaybeUniqMap_C :: (a -> a -> Maybe a) -> UniqMap k a -> UniqMap k a -> UniqMap k a
plusMaybeUniqMap_C f (UniqMap m1) (UniqMap m2) = UniqMap $
    plusMaybeUFM_C (\(_, v) (k', v') -> fmap (k',) (f v v')) m1 m2

plusUniqMapList :: [UniqMap k a] -> UniqMap k a
plusUniqMapList xs = UniqMap $ plusUFMList (coerce xs)

minusUniqMap :: UniqMap k a -> UniqMap k b -> UniqMap k a
minusUniqMap (UniqMap m1) (UniqMap m2) = UniqMap $ minusUFM m1 m2

intersectUniqMap :: UniqMap k a -> UniqMap k b -> UniqMap k a
intersectUniqMap (UniqMap m1) (UniqMap m2) = UniqMap $ intersectUFM m1 m2

disjointUniqMap :: UniqMap k a -> UniqMap k b -> Bool
disjointUniqMap (UniqMap m1) (UniqMap m2) = disjointUFM m1 m2

mapUniqMap :: (a -> b) -> UniqMap k a -> UniqMap k b
mapUniqMap f (UniqMap m) = UniqMap $ mapUFM (fmap f) m -- (,) k instance

filterUniqMap :: (a -> Bool) -> UniqMap k a -> UniqMap k a
filterUniqMap f (UniqMap m) = UniqMap $ filterUFM (f . snd) m

partitionUniqMap :: (a -> Bool) -> UniqMap k a -> (UniqMap k a, UniqMap k a)
partitionUniqMap f (UniqMap m) =
    coerce $ partitionUFM (f . snd) m

sizeUniqMap :: UniqMap k a -> Int
sizeUniqMap (UniqMap m) = sizeUFM m

elemUniqMap :: Uniquable k => k -> UniqMap k a -> Bool
elemUniqMap k (UniqMap m) = elemUFM k m

lookupUniqMap :: Uniquable k => UniqMap k a -> k -> Maybe a
lookupUniqMap (UniqMap m) k = fmap snd (lookupUFM m k)

lookupWithDefaultUniqMap :: Uniquable k => UniqMap k a -> a -> k -> a
lookupWithDefaultUniqMap (UniqMap m) a k = fromMaybe a (fmap snd (lookupUFM m k))

anyUniqMap :: (a -> Bool) -> UniqMap k a -> Bool
anyUniqMap f (UniqMap m) = anyUFM (f . snd) m

allUniqMap :: (a -> Bool) -> UniqMap k a -> Bool
allUniqMap f (UniqMap m) = allUFM (f . snd) m

nonDetEltsUniqMap :: UniqMap k a -> [(k, a)]
nonDetEltsUniqMap (UniqMap m) = nonDetEltsUFM m
