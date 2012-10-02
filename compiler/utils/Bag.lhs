%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Bag: an unordered collection with duplicates

\begin{code}
module Bag (
        Bag, -- abstract type

        emptyBag, unitBag, unionBags, unionManyBags,
        mapBag,
        elemBag, lengthBag,
        filterBag, partitionBag, partitionBagWith,
        concatBag, foldBag, foldrBag, foldlBag,
        isEmptyBag, isSingletonBag, consBag, snocBag, anyBag,
        listToBag, bagToList,
        foldrBagM, foldlBagM, mapBagM, mapBagM_,
        flatMapBagM, flatMapBagPairM,
        mapAndUnzipBagM, mapAccumBagLM
    ) where

#include "Typeable.h"

import Outputable
import Util

import MonadUtils
import Data.Data
import Data.List ( partition )

infixr 3 `consBag`
infixl 3 `snocBag`
\end{code}


\begin{code}
data Bag a
  = EmptyBag
  | UnitBag a
  | TwoBags (Bag a) (Bag a) -- INVARIANT: neither branch is empty
  | ListBag [a]             -- INVARIANT: the list is non-empty
    deriving Typeable

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
isEmptyBag _        = False -- NB invariants

isSingletonBag :: Bag a -> Bool
isSingletonBag EmptyBag      = False
isSingletonBag (UnitBag _)   = True
isSingletonBag (TwoBags _ _) = False          -- Neither is empty
isSingletonBag (ListBag xs)  = isSingleton xs

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag _    EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
    where sat1 = filterBag pred b1
          sat2 = filterBag pred b2
filterBag pred (ListBag vs)    = listToBag (filter pred vs)

anyBag :: (a -> Bool) -> Bag a -> Bool
anyBag _ EmptyBag        = False
anyBag p (UnitBag v)     = p v
anyBag p (TwoBags b1 b2) = anyBag p b1 || anyBag p b2
anyBag p (ListBag xs)    = any p xs

concatBag :: Bag (Bag a) -> Bag a
concatBag EmptyBag        = EmptyBag
concatBag (UnitBag b)     = b
concatBag (TwoBags b1 b2) = concatBag b1 `unionBags` concatBag b2
concatBag (ListBag bs)    = unionManyBags bs

partitionBag :: (a -> Bool) -> Bag a -> (Bag a {- Satisfy predictate -},
                                         Bag a {- Don't -})
partitionBag _    EmptyBag = (EmptyBag, EmptyBag)
partitionBag pred b@(UnitBag val)
    = if pred val then (b, EmptyBag) else (EmptyBag, b)
partitionBag pred (TwoBags b1 b2)
    = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
  where (sat1, fail1) = partitionBag pred b1
        (sat2, fail2) = partitionBag pred b2
partitionBag pred (ListBag vs) = (listToBag sats, listToBag fails)
  where (sats, fails) = partition pred vs


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
  where (sats, fails) = partitionWith pred vs

foldBag :: (r -> r -> r) -- Replace TwoBags with this; should be associative
        -> (a -> r)      -- Replace UnitBag with this
        -> r             -- Replace EmptyBag with this
        -> Bag a
        -> r

{- Standard definition
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x
foldBag t u e (TwoBags b1 b2) = (foldBag t u e b1) `t` (foldBag t u e b2)
foldBag t u e (ListBag xs)    = foldr (t.u) e xs
-}

-- More tail-recursive definition, exploiting associativity of "t"
foldBag _ _ e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x `t` e
foldBag t u e (TwoBags b1 b2) = foldBag t u (foldBag t u e b2) b1
foldBag t u e (ListBag xs)    = foldr (t.u) e xs

foldrBag :: (a -> r -> r) -> r
         -> Bag a
         -> r

foldrBag _ z EmptyBag        = z
foldrBag k z (UnitBag x)     = k x z
foldrBag k z (TwoBags b1 b2) = foldrBag k (foldrBag k z b2) b1
foldrBag k z (ListBag xs)    = foldr k z xs

foldlBag :: (r -> a -> r) -> r
         -> Bag a
         -> r

foldlBag _ z EmptyBag        = z
foldlBag k z (UnitBag x)     = k z x
foldlBag k z (TwoBags b1 b2) = foldlBag k (foldlBag k z b1) b2
foldlBag k z (ListBag xs)    = foldl k z xs

foldrBagM :: (Monad m) => (a -> b -> m b) -> b -> Bag a -> m b
foldrBagM _ z EmptyBag        = return z
foldrBagM k z (UnitBag x)     = k x z
foldrBagM k z (TwoBags b1 b2) = do { z' <- foldrBagM k z b2; foldrBagM k z' b1 }
foldrBagM k z (ListBag xs)    = foldrM k z xs

foldlBagM :: (Monad m) => (b -> a -> m b) -> b -> Bag a -> m b
foldlBagM _ z EmptyBag        = return z
foldlBagM k z (UnitBag x)     = k z x
foldlBagM k z (TwoBags b1 b2) = do { z' <- foldlBagM k z b1; foldlBagM k z' b2 }
foldlBagM k z (ListBag xs)    = foldlM k z xs

mapBag :: (a -> b) -> Bag a -> Bag b
mapBag _ EmptyBag        = EmptyBag
mapBag f (UnitBag x)     = UnitBag (f x)
mapBag f (TwoBags b1 b2) = TwoBags (mapBag f b1) (mapBag f b2)
mapBag f (ListBag xs)    = ListBag (map f xs)

mapBagM :: Monad m => (a -> m b) -> Bag a -> m (Bag b)
mapBagM _ EmptyBag        = return EmptyBag
mapBagM f (UnitBag x)     = do r <- f x
                               return (UnitBag r)
mapBagM f (TwoBags b1 b2) = do r1 <- mapBagM f b1
                               r2 <- mapBagM f b2
                               return (TwoBags r1 r2)
mapBagM f (ListBag    xs) = do rs <- mapM f xs
                               return (ListBag rs)

mapBagM_ :: Monad m => (a -> m b) -> Bag a -> m ()
mapBagM_ _ EmptyBag        = return ()
mapBagM_ f (UnitBag x)     = f x >> return ()
mapBagM_ f (TwoBags b1 b2) = mapBagM_ f b1 >> mapBagM_ f b2
mapBagM_ f (ListBag    xs) = mapM_ f xs

flatMapBagM :: Monad m => (a -> m (Bag b)) -> Bag a -> m (Bag b)
flatMapBagM _ EmptyBag        = return EmptyBag
flatMapBagM f (UnitBag x)     = f x
flatMapBagM f (TwoBags b1 b2) = do r1 <- flatMapBagM f b1
                                   r2 <- flatMapBagM f b2
                                   return (r1 `unionBags` r2)
flatMapBagM f (ListBag    xs) = foldrM k EmptyBag xs
  where
    k x b2 = do { b1 <- f x; return (b1 `unionBags` b2) }

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

mapAndUnzipBagM :: Monad m => (a -> m (b,c)) -> Bag a -> m (Bag b, Bag c)
mapAndUnzipBagM _ EmptyBag        = return (EmptyBag, EmptyBag)
mapAndUnzipBagM f (UnitBag x)     = do (r,s) <- f x
                                       return (UnitBag r, UnitBag s)
mapAndUnzipBagM f (TwoBags b1 b2) = do (r1,s1) <- mapAndUnzipBagM f b1
                                       (r2,s2) <- mapAndUnzipBagM f b2
                                       return (TwoBags r1 r2, TwoBags s1 s2)
mapAndUnzipBagM f (ListBag xs)    = do ts <- mapM f xs
                                       let (rs,ss) = unzip ts
                                       return (ListBag rs, ListBag ss)

mapAccumBagLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining funcction
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

listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag vs = ListBag vs

bagToList :: Bag a -> [a]
bagToList b = foldrBag (:) [] b
\end{code}

\begin{code}
instance (Outputable a) => Outputable (Bag a) where
    ppr bag = braces (pprWithCommas ppr (bagToList bag))

instance Data a => Data (Bag a) where
  gfoldl k z b = z listToBag `k` bagToList b -- traverse abstract type abstractly
  toConstr _   = abstractConstr $ "Bag("++show (typeOf (undefined::a))++")"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Bag"
  dataCast1 x  = gcast1 x
\end{code}

