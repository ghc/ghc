{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Bag: an unordered collection with duplicates
-}

{-# LANGUAGE ScopedTypeVariables, DeriveTraversable, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-x-data-list-nonempty-unzip #-}

module GHC.Data.Bag (
        Bag, -- abstract type

        emptyBag, unitBag, unionBags, unionManyBags,
        mapBag, pprBag,
        elemBag, lengthBag,
        filterBag, partitionBag, partitionBagWith,
        concatBag, catBagMaybes, foldBag_flip,
        isEmptyBag, isSingletonBag, consBag, snocBag, anyBag, allBag,
        listToBag, nonEmptyToBag, bagToList, headMaybe, mapAccumBagL,
        concatMapBag, concatMapBagPair, mapMaybeBag, mapMaybeBagM, unzipBag,
        mapBagM, mapBagM_, lookupBag,
        flatMapBagM, flatMapBagPairM,
        mapAndUnzipBagM, mapAccumBagLM,
        anyBagM, filterBagM
    ) where

import GHC.Prelude

import GHC.Exts ( IsList(..) )
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad
import Control.Monad
import Data.Data
import Data.Maybe( mapMaybe )
import Data.List ( partition, mapAccumL )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup ( (<>) )
import Control.Applicative( Alternative( (<|>) ) )
import Control.DeepSeq

infixr 3 `consBag`
infixl 3 `snocBag`

data Bag a
  = EmptyBag
  | UnitBag a
  | TwoBags (Bag a) (Bag a) -- INVARIANT: neither branch is empty
  | ListBag (NonEmpty a)
  deriving (Foldable, Functor, Traversable)

instance NFData a => NFData (Bag a) where
  rnf EmptyBag = ()
  rnf (UnitBag a) = rnf a
  rnf (TwoBags a b) = rnf a `seq` rnf b
  rnf (ListBag a) = rnf a

emptyBag :: Bag a
emptyBag = EmptyBag

unitBag :: a -> Bag a
unitBag  = UnitBag

lengthBag :: Bag a -> Int
lengthBag EmptyBag        = 0
lengthBag (UnitBag {})    = 1
lengthBag (TwoBags b1 b2) = lengthBag b1 + lengthBag b2
lengthBag (ListBag xs)    = length xs

elemBag :: Eq a => a -> Bag a -> Bool
elemBag _ EmptyBag        = False
elemBag x (UnitBag y)     = x == y
elemBag x (TwoBags b1 b2) = x `elemBag` b1 || x `elemBag` b2
elemBag x (ListBag ys)    = any (x ==) ys

unionManyBags :: [Bag a] -> Bag a
unionManyBags xs = foldr unionBags EmptyBag xs

-- This one is a bit stricter! The bag will get completely evaluated.

unionBags :: Bag a -> Bag a -> Bag a
unionBags EmptyBag b = b
unionBags b EmptyBag = b
unionBags b1 b2      = TwoBags b1 b2

consBag :: a -> Bag a -> Bag a
snocBag :: Bag a -> a -> Bag a

consBag elt bag = (unitBag elt) `unionBags` bag
snocBag bag elt = bag `unionBags` (unitBag elt)

isEmptyBag :: Bag a -> Bool
isEmptyBag EmptyBag = True
isEmptyBag _ = False

isSingletonBag :: Bag a -> Bool
isSingletonBag EmptyBag      = False
isSingletonBag (UnitBag _)   = True
isSingletonBag (TwoBags _ _) = False          -- Neither is empty
isSingletonBag (ListBag (_:|xs)) = null xs

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag _    EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
    where sat1 = filterBag pred b1
          sat2 = filterBag pred b2
filterBag pred (ListBag vs)    = listToBag (filter pred (toList vs))

filterBagM :: Monad m => (a -> m Bool) -> Bag a -> m (Bag a)
filterBagM _    EmptyBag = return EmptyBag
filterBagM pred b@(UnitBag val) = do
  flag <- pred val
  if flag then return b
          else return EmptyBag
filterBagM pred (TwoBags b1 b2) = do
  sat1 <- filterBagM pred b1
  sat2 <- filterBagM pred b2
  return (sat1 `unionBags` sat2)
filterBagM pred (ListBag vs) = do
  sat <- filterM pred (toList vs)
  return (listToBag sat)
{-# INLINEABLE filterBagM #-}

lookupBag :: Eq a => a -> Bag (a,b) -> Maybe b
lookupBag _ EmptyBag        = Nothing
lookupBag k (UnitBag kv)    = lookup_one k kv
lookupBag k (TwoBags b1 b2) = lookupBag k b1 <|> lookupBag k b2
lookupBag k (ListBag xs)    = foldr ((<|>) . lookup_one k) Nothing xs
{-# INLINEABLE lookupBag #-}

lookup_one :: Eq a => a -> (a,b) -> Maybe b
lookup_one k (k',v) | k==k'     = Just v
                    | otherwise = Nothing

allBag :: (a -> Bool) -> Bag a -> Bool
allBag _ EmptyBag        = True
allBag p (UnitBag v)     = p v
allBag p (TwoBags b1 b2) = allBag p b1 && allBag p b2
allBag p (ListBag xs)    = all p xs

anyBag :: (a -> Bool) -> Bag a -> Bool
anyBag _ EmptyBag        = False
anyBag p (UnitBag v)     = p v
anyBag p (TwoBags b1 b2) = anyBag p b1 || anyBag p b2
anyBag p (ListBag xs)    = any p xs

anyBagM :: Monad m => (a -> m Bool) -> Bag a -> m Bool
anyBagM _ EmptyBag        = return False
anyBagM p (UnitBag v)     = p v
anyBagM p (TwoBags b1 b2) = do flag <- anyBagM p b1
                               if flag then return True
                                       else anyBagM p b2
anyBagM p (ListBag xs)    = anyM p xs
{-# INLINEABLE anyBagM #-}

concatBag :: Bag (Bag a) -> Bag a
concatBag = foldr unionBags emptyBag

catBagMaybes :: Bag (Maybe a) -> Bag a
catBagMaybes bs = foldr add emptyBag bs
  where
    add Nothing rs = rs
    add (Just x) rs = x `consBag` rs

partitionBag :: (a -> Bool) -> Bag a -> (Bag a {- Satisfy predicate -},
                                         Bag a {- Don't -})
partitionBag _    EmptyBag = (EmptyBag, EmptyBag)
partitionBag pred b@(UnitBag val)
    = if pred val then (b, EmptyBag) else (EmptyBag, b)
partitionBag pred (TwoBags b1 b2)
    = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
  where (sat1, fail1) = partitionBag pred b1
        (sat2, fail2) = partitionBag pred b2
partitionBag pred (ListBag vs) = (listToBag sats, listToBag fails)
  where (sats, fails) = partition pred (toList vs)


partitionBagWith :: (a -> Either b c) -> Bag a
                    -> (Bag b {- Left  -},
                        Bag c {- Right -})
partitionBagWith _    EmptyBag = (EmptyBag, EmptyBag)
partitionBagWith pred (UnitBag val)
    = case pred val of
         Left a  -> (UnitBag a, EmptyBag)
         Right b -> (EmptyBag, UnitBag b)
partitionBagWith pred (TwoBags b1 b2)
    = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
  where (sat1, fail1) = partitionBagWith pred b1
        (sat2, fail2) = partitionBagWith pred b2
partitionBagWith pred (ListBag vs) = (listToBag sats, listToBag fails)
  where (sats, fails) = partitionWith pred (toList vs)

foldBag_flip :: (a -> b -> b) -> Bag a -> b -> b
-- Just foldr with flipped arguments,
-- so it can be chained more nicely
foldBag_flip k bag z = foldr k z bag

mapBag :: (a -> b) -> Bag a -> Bag b
mapBag = fmap

concatMapBag :: (a -> Bag b) -> Bag a -> Bag b
concatMapBag _ EmptyBag        = EmptyBag
concatMapBag f (UnitBag x)     = f x
concatMapBag f (TwoBags b1 b2) = unionBags (concatMapBag f b1) (concatMapBag f b2)
concatMapBag f (ListBag xs)    = foldr (unionBags . f) emptyBag xs

concatMapBagPair :: (a -> (Bag b, Bag c)) -> Bag a -> (Bag b, Bag c)
concatMapBagPair _ EmptyBag        = (EmptyBag, EmptyBag)
concatMapBagPair f (UnitBag x)     = f x
concatMapBagPair f (TwoBags b1 b2) = (unionBags r1 r2, unionBags s1 s2)
  where
    (r1, s1) = concatMapBagPair f b1
    (r2, s2) = concatMapBagPair f b2
concatMapBagPair f (ListBag xs)    = foldr go (emptyBag, emptyBag) xs
  where
    go a (s1, s2) = (unionBags r1 s1, unionBags r2 s2)
      where
        (r1, r2) = f a

mapMaybeBag :: (a -> Maybe b) -> Bag a -> Bag b
mapMaybeBag _ EmptyBag        = EmptyBag
mapMaybeBag f (UnitBag x)     = case f x of
                                  Nothing -> EmptyBag
                                  Just y  -> UnitBag y
mapMaybeBag f (TwoBags b1 b2) = unionBags (mapMaybeBag f b1) (mapMaybeBag f b2)
mapMaybeBag f (ListBag xs)    = listToBag $ mapMaybe f (toList xs)

mapMaybeBagM :: Monad m => (a -> m (Maybe b)) -> Bag a -> m (Bag b)
mapMaybeBagM _ EmptyBag        = return EmptyBag
mapMaybeBagM f (UnitBag x)     = do r <- f x
                                    return $ case r of
                                      Nothing -> EmptyBag
                                      Just y  -> UnitBag y
mapMaybeBagM f (TwoBags b1 b2) = do r1 <- mapMaybeBagM f b1
                                    r2 <- mapMaybeBagM f b2
                                    return $ unionBags r1 r2
mapMaybeBagM f (ListBag xs)    = listToBag <$> mapMaybeM f (toList xs)

mapBagM :: Monad m => (a -> m b) -> Bag a -> m (Bag b)
mapBagM _ EmptyBag        = return EmptyBag
mapBagM f (UnitBag x)     = do r <- f x
                               return (UnitBag r)
mapBagM f (TwoBags b1 b2) = do r1 <- mapBagM f b1
                               r2 <- mapBagM f b2
                               return (TwoBags r1 r2)
mapBagM f (ListBag    xs) = do rs <- mapM f xs
                               return (ListBag rs)
{-# INLINEABLE mapBagM #-}

mapBagM_ :: Monad m => (a -> m b) -> Bag a -> m ()
mapBagM_ _ EmptyBag        = return ()
mapBagM_ f (UnitBag x)     = f x >> return ()
mapBagM_ f (TwoBags b1 b2) = mapBagM_ f b1 >> mapBagM_ f b2
mapBagM_ f (ListBag    xs) = mapM_ f xs
{-# INLINEABLE mapBagM_ #-}

flatMapBagM :: Monad m => (a -> m (Bag b)) -> Bag a -> m (Bag b)
flatMapBagM _ EmptyBag        = return EmptyBag
flatMapBagM f (UnitBag x)     = f x
flatMapBagM f (TwoBags b1 b2) = do r1 <- flatMapBagM f b1
                                   r2 <- flatMapBagM f b2
                                   return (r1 `unionBags` r2)
flatMapBagM f (ListBag    xs) = foldrM k EmptyBag xs
  where
    k x b2 = do { b1 <- f x; return (b1 `unionBags` b2) }
{-# INLINEABLE flatMapBagM #-}

flatMapBagPairM :: Monad m => (a -> m (Bag b, Bag c)) -> Bag a -> m (Bag b, Bag c)
flatMapBagPairM _ EmptyBag        = return (EmptyBag, EmptyBag)
flatMapBagPairM f (UnitBag x)     = f x
flatMapBagPairM f (TwoBags b1 b2) = do (r1,s1) <- flatMapBagPairM f b1
                                       (r2,s2) <- flatMapBagPairM f b2
                                       return (r1 `unionBags` r2, s1 `unionBags` s2)
flatMapBagPairM f (ListBag    xs) = foldrM k (EmptyBag, EmptyBag) xs
  where
    k x (r2,s2) = do { (r1,s1) <- f x
                     ; return (r1 `unionBags` r2, s1 `unionBags` s2) }
{-# INLINEABLE flatMapBagPairM #-}

mapAndUnzipBagM :: Monad m => (a -> m (b,c)) -> Bag a -> m (Bag b, Bag c)
mapAndUnzipBagM _ EmptyBag        = return (EmptyBag, EmptyBag)
mapAndUnzipBagM f (UnitBag x)     = do (r,s) <- f x
                                       return (UnitBag r, UnitBag s)
mapAndUnzipBagM f (TwoBags b1 b2) = do (r1,s1) <- mapAndUnzipBagM f b1
                                       (r2,s2) <- mapAndUnzipBagM f b2
                                       return (TwoBags r1 r2, TwoBags s1 s2)
mapAndUnzipBagM f (ListBag xs)    = do ts <- mapM f xs
                                       let (rs,ss) = NE.unzip ts
                                       return (ListBag rs, ListBag ss)
{-# INLINEABLE mapAndUnzipBagM #-}

mapAccumBagL ::(acc -> x -> (acc, y)) -- ^ combining function
            -> acc                    -- ^ initial state
            -> Bag x                  -- ^ inputs
            -> (acc, Bag y)           -- ^ final state, outputs
mapAccumBagL _ s EmptyBag        = (s, EmptyBag)
mapAccumBagL f s (UnitBag x)     = let (s1, x1) = f s x in (s1, UnitBag x1)
mapAccumBagL f s (TwoBags b1 b2) = let (s1, b1') = mapAccumBagL f s  b1
                                       (s2, b2') = mapAccumBagL f s1 b2
                                   in (s2, TwoBags b1' b2')
mapAccumBagL f s (ListBag xs)    = let (s', xs') = mapAccumL f s xs
                                   in (s', ListBag xs')

mapAccumBagLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> Bag x                    -- ^ inputs
            -> m (acc, Bag y)           -- ^ final state, outputs
mapAccumBagLM _ s EmptyBag        = return (s, EmptyBag)
mapAccumBagLM f s (UnitBag x)     = do { (s1, x1) <- f s x; return (s1, UnitBag x1) }
mapAccumBagLM f s (TwoBags b1 b2) = do { (s1, b1') <- mapAccumBagLM f s  b1
                                       ; (s2, b2') <- mapAccumBagLM f s1 b2
                                       ; return (s2, TwoBags b1' b2') }
mapAccumBagLM f s (ListBag xs)    = do { (s', xs') <- mapAccumLM f s xs
                                       ; return (s', ListBag xs') }
{-# INLINEABLE mapAccumBagLM #-}

listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag [x] = UnitBag x
listToBag (x:xs) = ListBag (x:|xs)

nonEmptyToBag :: NonEmpty a -> Bag a
nonEmptyToBag (x :| []) = UnitBag x
nonEmptyToBag xs = ListBag xs

bagToList :: Bag a -> [a]
bagToList b = foldr (:) [] b

unzipBag :: Bag (a, b) -> (Bag a, Bag b)
unzipBag EmptyBag = (EmptyBag, EmptyBag)
unzipBag (UnitBag (a, b)) = (UnitBag a, UnitBag b)
unzipBag (TwoBags xs1 xs2) = (TwoBags as1 as2, TwoBags bs1 bs2)
  where
    (as1, bs1) = unzipBag xs1
    (as2, bs2) = unzipBag xs2
unzipBag (ListBag xs) = (ListBag as, ListBag bs)
  where
    (as, bs) = NE.unzip xs

headMaybe :: Bag a -> Maybe a
headMaybe EmptyBag = Nothing
headMaybe (UnitBag v) = Just v
headMaybe (TwoBags b1 _) = headMaybe b1
headMaybe (ListBag (v:|_)) = Just v

instance (Outputable a) => Outputable (Bag a) where
    ppr = pprBag

pprBag :: Outputable a => Bag a -> SDoc
pprBag bag = braces (pprWithCommas ppr (bagToList bag))

instance Data a => Data (Bag a) where
  gfoldl k z b = z listToBag `k` bagToList b -- traverse abstract type abstractly
  toConstr _   = abstractConstr $ "Bag("++show (typeOf (undefined::a))++")"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Bag"
  dataCast1 x  = gcast1 x

instance IsList (Bag a) where
  type Item (Bag a) = a
  fromList = listToBag
  toList   = bagToList

instance Semigroup (Bag a) where
  (<>) = unionBags

instance Monoid (Bag a) where
  mempty = emptyBag
