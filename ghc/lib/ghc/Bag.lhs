%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Bags]{@Bag@: an unordered collection with duplicates}

\begin{code}
module Bag (
	Bag,	-- abstract type

	emptyBag, unitBag, unionBags, unionManyBags,
#if ! defined(COMPILING_GHC)
	elemBag,
#endif
	filterBag, partitionBag,
	isEmptyBag, snocBag, listToBag, bagToList
    ) where

#if defined(COMPILING_GHC)
import Id		( Id )
import Outputable
import Pretty
import Util
#endif

data Bag a
  = EmptyBag
  | UnitBag	a
  | TwoBags	(Bag a) (Bag a)	-- The ADT guarantees that at least
				-- one branch is non-empty.
  | ListOfBags	[Bag a]		-- The list is non-empty

emptyBag = EmptyBag
unitBag  = UnitBag

#if ! defined(COMPILING_GHC)
-- not used in GHC
elemBag :: Eq a => a -> Bag a -> Bool
elemBag x EmptyBag        = False
elemBag x (UnitBag y)     = x==y
elemBag x (TwoBags b1 b2) = x `elemBag` b1 || x `elemBag` b2
elemBag x (ListOfBags bs) = any (x `elemBag`) bs
#endif

unionManyBags [] = EmptyBag
unionManyBags xs = ListOfBags xs

-- This one is a bit stricter! The bag will get completely evaluated.


unionBags EmptyBag b = b
unionBags b EmptyBag = b
unionBags b1 b2      = TwoBags b1 b2

snocBag :: Bag a -> a -> Bag a
snocBag bag elt = bag `unionBags` (unitBag elt)

isEmptyBag EmptyBag	    = True
isEmptyBag (TwoBags b1 b2)  = isEmptyBag b1 && isEmptyBag b2	-- Paranoid, but safe
isEmptyBag (ListOfBags bs)  = all isEmptyBag bs
isEmptyBag other	    = False

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag pred EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
			       where
				 sat1 = filterBag pred b1
				 sat2 = filterBag pred b2
filterBag pred (ListOfBags bs) = ListOfBags sats
			        where
				 sats = [filterBag pred b | b <- bs]


partitionBag :: (a -> Bool) -> Bag a -> (Bag a {- Satisfy predictate -}, 
					 Bag a {- Don't -})
partitionBag pred EmptyBag = (EmptyBag, EmptyBag)
partitionBag pred b@(UnitBag val) = if pred val then (b, EmptyBag) else (EmptyBag, b)
partitionBag pred (TwoBags b1 b2) = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
				  where
				    (sat1,fail1) = partitionBag pred b1
				    (sat2,fail2) = partitionBag pred b2
partitionBag pred (ListOfBags bs) = (ListOfBags sats, ListOfBags fails)
				  where
				    (sats, fails) = unzip [partitionBag pred b | b <- bs]


listToBag :: [a] -> Bag a
listToBag lst = foldr TwoBags EmptyBag (map UnitBag lst)

bagToList :: Bag a -> [a]
bagToList b = b_to_l b []
  where
    -- (b_to_l b xs) flattens b and puts xs on the end.
    b_to_l EmptyBag 	   xs = xs
    b_to_l (UnitBag x)	   xs = x:xs
    b_to_l (TwoBags b1 b2) xs = b_to_l b1 (b_to_l b2 xs)
    b_to_l (ListOfBags bs) xs = foldr b_to_l xs bs 
\end{code}

\begin{code}
#if defined(COMPILING_GHC)

instance (Outputable a) => Outputable (Bag a) where
    ppr sty EmptyBag	    = ppStr "emptyBag"
    ppr sty (UnitBag a)     = ppr sty a
    ppr sty (TwoBags b1 b2) = ppCat [ppr sty b1, pp'SP, ppr sty b2]
    ppr sty (ListOfBags bs) = ppCat [ppLbrack, interpp'SP sty bs, ppRbrack]

#endif {- COMPILING_GHC -}
\end{code}
