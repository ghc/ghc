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
	isEmptyBag, consBag, snocBag,
	listToBag, bagToList
    ) where

#include "HsVersions.h"

import Outputable
import List		( partition )
\end{code}


\begin{code}
data Bag a
  = EmptyBag
  | UnitBag	a
  | TwoBags	(Bag a) (Bag a)	-- The ADT guarantees that at least
				-- one branch is non-empty
  | ListBag	[a]		-- The list is non-empty
  | ListOfBags	[Bag a]		-- The list is non-empty

emptyBag = EmptyBag
unitBag  = UnitBag

elemBag :: Eq a => a -> Bag a -> Bool

elemBag x EmptyBag        = False
elemBag x (UnitBag y)     = x==y
elemBag x (TwoBags b1 b2) = x `elemBag` b1 || x `elemBag` b2
elemBag x (ListBag ys)    = any (x ==) ys
elemBag x (ListOfBags bs) = any (x `elemBag`) bs

unionManyBags [] = EmptyBag
unionManyBags xs = ListOfBags xs

-- This one is a bit stricter! The bag will get completely evaluated.

unionBags EmptyBag b = b
unionBags b EmptyBag = b
unionBags b1 b2      = TwoBags b1 b2

consBag :: a -> Bag a -> Bag a
snocBag :: Bag a -> a -> Bag a

consBag elt bag = (unitBag elt) `unionBags` bag
snocBag bag elt = bag `unionBags` (unitBag elt)

isEmptyBag EmptyBag	    = True
isEmptyBag (UnitBag x)	    = False
isEmptyBag (TwoBags b1 b2)  = isEmptyBag b1 && isEmptyBag b2	-- Paranoid, but safe
isEmptyBag (ListBag xs)     = null xs				-- Paranoid, but safe
isEmptyBag (ListOfBags bs)  = all isEmptyBag bs

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag pred EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
			       where
				 sat1 = filterBag pred b1
				 sat2 = filterBag pred b2
filterBag pred (ListBag vs)    = listToBag (filter pred vs)
filterBag pred (ListOfBags bs) = ListOfBags sats
				where
				 sats = [filterBag pred b | b <- bs]

concatBag :: Bag (Bag a) -> Bag a

concatBag EmptyBag 	    = EmptyBag
concatBag (UnitBag b)       = b
concatBag (TwoBags b1 b2)   = concatBag b1 `TwoBags` concatBag b2
concatBag (ListBag bs)	    = ListOfBags bs
concatBag (ListOfBags bbs)  = ListOfBags (map concatBag bbs)

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
partitionBag pred (ListOfBags bs) = (ListOfBags sats, ListOfBags fails)
				  where
				    (sats, fails) = unzip [partitionBag pred b | b <- bs]


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
foldBag t u e (ListOfBags bs) = foldr (\b r -> foldBag e u t b `t` r) e bs
-}

-- More tail-recursive definition, exploiting associativity of "t"
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x `t` e
foldBag t u e (TwoBags b1 b2) = foldBag t u (foldBag t u e b2) b1
foldBag t u e (ListBag xs)    = foldr (t.u) e xs
foldBag t u e (ListOfBags bs) = foldr (\b r -> foldBag t u r b) e bs

foldrBag :: (a -> r -> r) -> r
	 -> Bag a
	 -> r

foldrBag k z EmptyBag        = z
foldrBag k z (UnitBag x)     = k x z
foldrBag k z (TwoBags b1 b2) = foldrBag k (foldrBag k z b2) b1
foldrBag k z (ListBag xs)    = foldr k z xs
foldrBag k z (ListOfBags bs) = foldr (\b r -> foldrBag k r b) z bs

foldlBag :: (r -> a -> r) -> r
	 -> Bag a
	 -> r

foldlBag k z EmptyBag        = z
foldlBag k z (UnitBag x)     = k z x
foldlBag k z (TwoBags b1 b2) = foldlBag k (foldlBag k z b1) b2
foldlBag k z (ListBag xs)    = foldl k z xs
foldlBag k z (ListOfBags bs) = foldl (\r b -> foldlBag k r b) z bs


mapBag :: (a -> b) -> Bag a -> Bag b
mapBag f EmptyBag 	 = EmptyBag
mapBag f (UnitBag x)     = UnitBag (f x)
mapBag f (TwoBags b1 b2) = TwoBags (mapBag f b1) (mapBag f b2) 
mapBag f (ListBag xs)    = ListBag (map f xs)
mapBag f (ListOfBags bs) = ListOfBags (map (mapBag f) bs)


listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag vs = ListBag vs

bagToList :: Bag a -> [a]
bagToList b = foldrBag (:) [] b
\end{code}

\begin{code}
instance (Outputable a) => Outputable (Bag a) where
    ppr EmptyBag	= ptext SLIT("emptyBag")
    ppr (UnitBag a)     = ppr a
    ppr (TwoBags b1 b2) = hsep [ppr b1 <> comma, ppr b2]
    ppr (ListBag as)    = interpp'SP as
    ppr (ListOfBags bs) = brackets (interpp'SP bs)

\end{code}
