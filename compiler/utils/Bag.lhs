%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Bags]{@Bag@: an unordered collection with duplicates}

\begin{code}
module Bag (
	Bag,	-- abstract type

	emptyBag, unitBag, unionBags, unionManyBags,
	mapBag,
	elemBag,
	filterBag, partitionBag, concatBag, foldBag, foldrBag, foldlBag,
	isEmptyBag, isSingletonBag, consBag, snocBag, anyBag,
	listToBag, bagToList, 
	mapBagM, mapAndUnzipBagM
    ) where

#include "HsVersions.h"

import Outputable
import Util		( isSingleton )
import List		( partition )
\end{code}


\begin{code}
data Bag a
  = EmptyBag
  | UnitBag	a
  | TwoBags	(Bag a) (Bag a)	-- INVARIANT: neither branch is empty
  | ListBag	[a]		-- INVARIANT: the list is non-empty

emptyBag = EmptyBag
unitBag  = UnitBag

elemBag :: Eq a => a -> Bag a -> Bool

elemBag x EmptyBag        = False
elemBag x (UnitBag y)     = x==y
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

isEmptyBag EmptyBag = True
isEmptyBag other    = False	-- NB invariants

isSingletonBag :: Bag a -> Bool
isSingletonBag EmptyBag	    	= False
isSingletonBag (UnitBag x)	= True
isSingletonBag (TwoBags b1 b2)  = False		-- Neither is empty
isSingletonBag (ListBag xs)     = isSingleton xs

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag pred EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
			       where
				 sat1 = filterBag pred b1
				 sat2 = filterBag pred b2
filterBag pred (ListBag vs)    = listToBag (filter pred vs)

anyBag :: (a -> Bool) -> Bag a -> Bool
anyBag p EmptyBag        = False
anyBag p (UnitBag v)     = p v
anyBag p (TwoBags b1 b2) = anyBag p b1 || anyBag p b2
anyBag p (ListBag xs)    = any p xs

concatBag :: Bag (Bag a) -> Bag a
concatBag EmptyBag 	    = EmptyBag
concatBag (UnitBag b)       = b
concatBag (TwoBags b1 b2)   = concatBag b1 `unionBags` concatBag b2
concatBag (ListBag bs)	    = unionManyBags bs

partitionBag :: (a -> Bool) -> Bag a -> (Bag a {- Satisfy predictate -},
					 Bag a {- Don't -})
partitionBag pred EmptyBag = (EmptyBag, EmptyBag)
partitionBag pred b@(UnitBag val) = if pred val then (b, EmptyBag) else (EmptyBag, b)
partitionBag pred (TwoBags b1 b2) = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
				  where
				    (sat1,fail1) = partitionBag pred b1
				    (sat2,fail2) = partitionBag pred b2
partitionBag pred (ListBag vs)	  = (listToBag sats, listToBag fails)
				  where
				    (sats,fails) = partition pred vs


foldBag :: (r -> r -> r)	-- Replace TwoBags with this; should be associative
	-> (a -> r)		-- Replace UnitBag with this
	-> r			-- Replace EmptyBag with this
	-> Bag a
	-> r

{- Standard definition
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x
foldBag t u e (TwoBags b1 b2) = (foldBag t u e b1) `t` (foldBag t u e b2)
foldBag t u e (ListBag xs)    = foldr (t.u) e xs
-}

-- More tail-recursive definition, exploiting associativity of "t"
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x `t` e
foldBag t u e (TwoBags b1 b2) = foldBag t u (foldBag t u e b2) b1
foldBag t u e (ListBag xs)    = foldr (t.u) e xs

foldrBag :: (a -> r -> r) -> r
	 -> Bag a
	 -> r

foldrBag k z EmptyBag        = z
foldrBag k z (UnitBag x)     = k x z
foldrBag k z (TwoBags b1 b2) = foldrBag k (foldrBag k z b2) b1
foldrBag k z (ListBag xs)    = foldr k z xs

foldlBag :: (r -> a -> r) -> r
	 -> Bag a
	 -> r

foldlBag k z EmptyBag        = z
foldlBag k z (UnitBag x)     = k z x
foldlBag k z (TwoBags b1 b2) = foldlBag k (foldlBag k z b1) b2
foldlBag k z (ListBag xs)    = foldl k z xs


mapBag :: (a -> b) -> Bag a -> Bag b
mapBag f EmptyBag 	 = EmptyBag
mapBag f (UnitBag x)     = UnitBag (f x)
mapBag f (TwoBags b1 b2) = TwoBags (mapBag f b1) (mapBag f b2) 
mapBag f (ListBag xs)    = ListBag (map f xs)

mapBagM :: Monad m => (a -> m b) -> Bag a -> m (Bag b)
mapBagM f EmptyBag 	  = return EmptyBag
mapBagM f (UnitBag x)     = do { r <- f x; return (UnitBag r) }
mapBagM f (TwoBags b1 b2) = do { r1 <- mapBagM f b1; r2 <- mapBagM f b2; return (TwoBags r1 r2) }
mapBagM f (ListBag    xs) = do { rs <- mapM    f xs; return (ListBag rs) }

mapAndUnzipBagM :: Monad m => (a -> m (b,c)) -> Bag a -> m (Bag b, Bag c)
mapAndUnzipBagM f EmptyBag 	  = return (EmptyBag, EmptyBag)
mapAndUnzipBagM f (UnitBag x)     = do { (r,s) <- f x; return (UnitBag r, UnitBag s) }
mapAndUnzipBagM f (TwoBags b1 b2) = do	{ (r1,s1) <- mapAndUnzipBagM f b1
					; (r2,s2) <- mapAndUnzipBagM f b2
					; return (TwoBags r1 r2, TwoBags s1 s2) }
mapAndUnzipBagM f (ListBag    xs) = do	{ ts <- mapM f xs
					; let (rs,ss) = unzip ts
					; return (ListBag rs, ListBag ss) }

listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag vs = ListBag vs

bagToList :: Bag a -> [a]
bagToList b = foldrBag (:) [] b
\end{code}

\begin{code}
instance (Outputable a) => Outputable (Bag a) where
    ppr bag = char '<' <> pprWithCommas ppr (bagToList bag) <> char '>'
\end{code}
