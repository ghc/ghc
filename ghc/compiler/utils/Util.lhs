%
% (c) The University of Glasgow 1992-2002
%
\section[Util]{Highly random utility functions}

\begin{code}
module Util (

	-- general list processing
	zipEqual, zipWithEqual, zipWith3Equal, zipWith4Equal,
        zipLazy, stretchZipWith,
	mapAndUnzip, mapAndUnzip3,
	nOfThem, 
	lengthExceeds, lengthIs, lengthAtLeast, listLengthCmp, atLength,
	isSingleton, only,
	notNull, snocView,

	isIn, isn'tIn,

	-- for-loop
	nTimes,

	-- sorting
	sortLt, naturalMergeSortLe,

	-- transitive closures
	transitiveClosure,

	-- accumulating
	mapAccumL, mapAccumR, mapAccumB, 
	foldl2, count,
	
	takeList, dropList, splitAtList,

	-- comparisons
	eqListBy, equalLength, compareLength,
	thenCmp, cmpList, prefixMatch, suffixMatch, maybePrefixMatch,

	-- strictness
	foldl', seqList,

	-- pairs
	unzipWith,

	global,

	-- module names
	looksLikeModuleName,
	
	toArgs
    ) where

#include "../includes/config.h"
#include "HsVersions.h"

import Panic		( panic, trace )
import FastTypes

#if __GLASGOW_HASKELL__ <= 408
import EXCEPTION	( catchIO, justIoErrors, raiseInThread )
#endif
import DATA_IOREF	( IORef, newIORef )
import UNSAFE_IO	( unsafePerformIO )

import qualified List	( elem, notElem )

#ifndef DEBUG
import List		( zipWith4 )
#endif

import Char		( isUpper, isAlphaNum, isSpace )

infixr 9 `thenCmp`
\end{code}

%************************************************************************
%*									*
\subsection{The Eager monad}
%*									*
%************************************************************************

The @Eager@ monad is just an encoding of continuation-passing style,
used to allow you to express "do this and then that", mainly to avoid
space leaks. It's done with a type synonym to save bureaucracy.

\begin{code}
#if NOT_USED

type Eager ans a = (a -> ans) -> ans

runEager :: Eager a a -> a
runEager m = m (\x -> x)

appEager :: Eager ans a -> (a -> ans) -> ans
appEager m cont = m cont

thenEager :: Eager ans a -> (a -> Eager ans b) -> Eager ans b
thenEager m k cont = m (\r -> k r cont)

returnEager :: a -> Eager ans a
returnEager v cont = cont v

mapEager :: (a -> Eager ans b) -> [a] -> Eager ans [b]
mapEager f [] = returnEager []
mapEager f (x:xs) = f x			`thenEager` \ y ->
		    mapEager f xs	`thenEager` \ ys ->
		    returnEager (y:ys)
#endif
\end{code}

%************************************************************************
%*									*
\subsection{A for loop}
%*									*
%************************************************************************

\begin{code}
-- Compose a function with itself n times.  (nth rather than twice)
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
\end{code}

%************************************************************************
%*									*
\subsection[Utils-lists]{General list processing}
%*									*
%************************************************************************

A paranoid @zip@ (and some @zipWith@ friends) that checks the lists
are of equal length.  Alastair Reid thinks this should only happen if
DEBUGging on; hey, why not?

\begin{code}
zipEqual	:: String -> [a] -> [b] -> [(a,b)]
zipWithEqual	:: String -> (a->b->c) -> [a]->[b]->[c]
zipWith3Equal	:: String -> (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith4Equal	:: String -> (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]

#ifndef DEBUG
zipEqual      _ = zip
zipWithEqual  _ = zipWith
zipWith3Equal _ = zipWith3
zipWith4Equal _ = zipWith4
#else
zipEqual msg []     []     = []
zipEqual msg (a:as) (b:bs) = (a,b) : zipEqual msg as bs
zipEqual msg as     bs     = panic ("zipEqual: unequal lists:"++msg)

zipWithEqual msg z (a:as) (b:bs)=  z a b : zipWithEqual msg z as bs
zipWithEqual msg _ [] []	=  []
zipWithEqual msg _ _ _		=  panic ("zipWithEqual: unequal lists:"++msg)

zipWith3Equal msg z (a:as) (b:bs) (c:cs)
				=  z a b c : zipWith3Equal msg z as bs cs
zipWith3Equal msg _ [] []  []	=  []
zipWith3Equal msg _ _  _   _	=  panic ("zipWith3Equal: unequal lists:"++msg)

zipWith4Equal msg z (a:as) (b:bs) (c:cs) (d:ds)
				=  z a b c d : zipWith4Equal msg z as bs cs ds
zipWith4Equal msg _ [] [] [] []	=  []
zipWith4Equal msg _ _  _  _  _	=  panic ("zipWith4Equal: unequal lists:"++msg)
#endif
\end{code}

\begin{code}
-- zipLazy is lazy in the second list (observe the ~)

zipLazy :: [a] -> [b] -> [(a,b)]
zipLazy [] ys = []
zipLazy (x:xs) ~(y:ys) = (x,y) : zipLazy xs ys
\end{code}


\begin{code}
stretchZipWith :: (a -> Bool) -> b -> (a->b->c) -> [a] -> [b] -> [c]
-- (stretchZipWith p z f xs ys) stretches ys by inserting z in 
-- the places where p returns *True*

stretchZipWith p z f [] ys = []
stretchZipWith p z f (x:xs) ys
  | p x       = f x z : stretchZipWith p z f xs ys
  | otherwise = case ys of
		  []     -> []
		  (y:ys) -> f x y : stretchZipWith p z f xs ys
\end{code}


\begin{code}
mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])

mapAndUnzip f [] = ([],[])
mapAndUnzip f (x:xs)
  = let
	(r1,  r2)  = f x
	(rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

mapAndUnzip3 :: (a -> (b, c, d)) -> [a] -> ([b], [c], [d])

mapAndUnzip3 f [] = ([],[],[])
mapAndUnzip3 f (x:xs)
  = let
	(r1,  r2,  r3)  = f x
	(rs1, rs2, rs3) = mapAndUnzip3 f xs
    in
    (r1:rs1, r2:rs2, r3:rs3)
\end{code}

\begin{code}
nOfThem :: Int -> a -> [a]
nOfThem n thing = replicate n thing

-- 'atLength atLen atEnd ls n' unravels list 'ls' to position 'n';
-- specification:
--
--  atLength atLenPred atEndPred ls n
--   | n < 0         = atLenPred n
--   | length ls < n = atEndPred (n - length ls)
--   | otherwise     = atLenPred (drop n ls)
--
atLength :: ([a] -> b)
         -> (Int -> b)
         -> [a]
         -> Int
         -> b
atLength atLenPred atEndPred ls n 
  | n < 0     = atEndPred n 
  | otherwise = go n ls
  where
    go n [] = atEndPred n
    go 0 ls = atLenPred ls
    go n (_:xs) = go (n-1) xs

-- special cases.
lengthExceeds :: [a] -> Int -> Bool
-- (lengthExceeds xs n) = (length xs > n)
lengthExceeds = atLength notNull (const False)

lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast = atLength notNull (== 0)

lengthIs :: [a] -> Int -> Bool
lengthIs = atLength null (==0)

listLengthCmp :: [a] -> Int -> Ordering 
listLengthCmp = atLength atLen atEnd 
 where
  atEnd 0      = EQ
  atEnd x
   | x > 0     = LT -- not yet seen 'n' elts, so list length is < n.
   | otherwise = GT

  atLen []     = EQ
  atLen _      = GT

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton  _  = False

notNull :: [a] -> Bool
notNull [] = False
notNull _  = True

snocView :: [a] -> Maybe ([a],a)
	-- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
	    where
		-- Invariant: second arg is non-empty
	      go acc [x]    = Just (reverse acc, x)
	      go acc (x:xs) = go (x:acc) xs

only :: [a] -> a
#ifdef DEBUG
only [a] = a
#else
only (a:_) = a
#endif
\end{code}

Debugging/specialising versions of \tr{elem} and \tr{notElem}

\begin{code}
isIn, isn'tIn :: (Eq a) => String -> a -> [a] -> Bool

# ifndef DEBUG
isIn    msg x ys = elem__    x ys
isn'tIn msg x ys = notElem__ x ys

--these are here to be SPECIALIZEd (automagically)
elem__ _ []	= False
elem__ x (y:ys)	= x==y || elem__ x ys

notElem__ x []	   =  True
notElem__ x (y:ys) =  x /= y && notElem__ x ys

# else /* DEBUG */
isIn msg x ys
  = elem (_ILIT 0) x ys
  where
    elem i _ []	    = False
    elem i x (y:ys)
      | i ># _ILIT 100 = trace ("Over-long elem in " ++ msg) $
			 x `List.elem` (y:ys)
      | otherwise      = x == y || elem (i +# _ILIT(1)) x ys

isn'tIn msg x ys
  = notElem (_ILIT 0) x ys
  where
    notElem i x [] =  True
    notElem i x (y:ys)
      | i ># _ILIT 100 = trace ("Over-long notElem in " ++ msg) $
		         x `List.notElem` (y:ys)
      | otherwise      =  x /= y && notElem (i +# _ILIT(1)) x ys
# endif /* DEBUG */
\end{code}

%************************************************************************
%*									*
\subsection[Utils-sorting]{Sorting}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection[Utils-quicksorting]{Quicksorts}
%*									*
%************************************************************************

\begin{code}
#if NOT_USED

-- tail-recursive, etc., "quicker sort" [as per Meira thesis]
quicksort :: (a -> a -> Bool)		-- Less-than predicate
	  -> [a]			-- Input list
	  -> [a]			-- Result list in increasing order

quicksort lt []      = []
quicksort lt [x]     = [x]
quicksort lt (x:xs)  = split x [] [] xs
  where
    split x lo hi []		     = quicksort lt lo ++ (x : quicksort lt hi)
    split x lo hi (y:ys) | y `lt` x  = split x (y:lo) hi ys
			 | True      = split x lo (y:hi) ys
#endif
\end{code}

Quicksort variant from Lennart's Haskell-library contribution.  This
is a {\em stable} sort.

\begin{code}
sortLt :: (a -> a -> Bool) 		-- Less-than predicate
       -> [a] 				-- Input list
       -> [a]				-- Result list

sortLt lt l = qsort lt   l []

-- qsort is stable and does not concatenate.
qsort :: (a -> a -> Bool)	-- Less-than predicate
      -> [a]			-- xs, Input list
      -> [a]			-- r,  Concatenate this list to the sorted input list
      -> [a]			-- Result = sort xs ++ r

qsort lt []     r = r
qsort lt [x]    r = x:r
qsort lt (x:xs) r = qpart lt x xs [] [] r

-- qpart partitions and sorts the sublists
-- rlt contains things less than x,
-- rge contains the ones greater than or equal to x.
-- Both have equal elements reversed with respect to the original list.

qpart lt x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort lt rlt (x : rqsort lt rge r)

qpart lt x (y:ys) rlt rge r =
    if lt y x then
	-- y < x
	qpart lt x ys (y:rlt) rge r
    else
	-- y >= x
	qpart lt x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort lt []     r = r
rqsort lt [x]    r = x:r
rqsort lt (x:xs) r = rqpart lt x xs [] [] r

rqpart lt x [] rle rgt r =
    qsort lt rle (x : qsort lt rgt r)

rqpart lt x (y:ys) rle rgt r =
    if lt x y then
	-- y > x
	rqpart lt x ys rle (y:rgt) r
    else
	-- y <= x
	rqpart lt x ys (y:rle) rgt r
\end{code}

%************************************************************************
%*									*
\subsubsection[Utils-dull-mergesort]{A rather dull mergesort}
%*									*
%************************************************************************

\begin{code}
#if NOT_USED
mergesort :: (a -> a -> Ordering) -> [a] -> [a]

mergesort cmp xs = merge_lists (split_into_runs [] xs)
  where
    a `le` b = case cmp a b of { LT -> True;  EQ -> True; GT -> False }
    a `ge` b = case cmp a b of { LT -> False; EQ -> True; GT -> True  }

    split_into_runs []        []	    	= []
    split_into_runs run       []	    	= [run]
    split_into_runs []        (x:xs)		= split_into_runs [x] xs
    split_into_runs [r]       (x:xs) | x `ge` r = split_into_runs [r,x] xs
    split_into_runs rl@(r:rs) (x:xs) | x `le` r = split_into_runs (x:rl) xs
				     | True     = rl : (split_into_runs [x] xs)

    merge_lists []	 = []
    merge_lists (x:xs)   = merge x (merge_lists xs)

    merge [] ys = ys
    merge xs [] = xs
    merge xl@(x:xs) yl@(y:ys)
      = case cmp x y of
	  EQ  -> x : y : (merge xs ys)
	  LT  -> x : (merge xs yl)
	  GT -> y : (merge xl ys)
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection[Utils-Carsten-mergesort]{A mergesort from Carsten}
%*									*
%************************************************************************

\begin{display}
Date: Mon, 3 May 93 20:45:23 +0200
From: Carsten Kehler Holst <kehler@cs.chalmers.se>
To: partain@dcs.gla.ac.uk
Subject: natural merge sort beats quick sort [ and it is prettier ]

Here is a piece of Haskell code that I'm rather fond of. See it as an
attempt to get rid of the ridiculous quick-sort routine. group is
quite useful by itself I think it was John's idea originally though I
believe the lazy version is due to me [surprisingly complicated].
gamma [used to be called] is called gamma because I got inspired by
the Gamma calculus. It is not very close to the calculus but does
behave less sequentially than both foldr and foldl. One could imagine
a version of gamma that took a unit element as well thereby avoiding
the problem with empty lists.

I've tried this code against

   1) insertion sort - as provided by haskell
   2) the normal implementation of quick sort
   3) a deforested version of quick sort due to Jan Sparud
   4) a super-optimized-quick-sort of Lennart's

If the list is partially sorted both merge sort and in particular
natural merge sort wins. If the list is random [ average length of
rising subsequences = approx 2 ] mergesort still wins and natural
merge sort is marginally beaten by Lennart's soqs. The space
consumption of merge sort is a bit worse than Lennart's quick sort
approx a factor of 2. And a lot worse if Sparud's bug-fix [see his
fpca article ] isn't used because of group.

have fun
Carsten
\end{display}

\begin{code}
group :: (a -> a -> Bool) -> [a] -> [[a]]

{-
Date: Mon, 12 Feb 1996 15:09:41 +0000
From: Andy Gill <andy@dcs.gla.ac.uk>

Here is a `better' definition of group.
-}
group p []     = []
group p (x:xs) = group' xs x x (x :)
  where
    group' []     _     _     s  = [s []]
    group' (x:xs) x_min x_max s 
	| not (x `p` x_max) = group' xs x_min x (s . (x :)) 
	| x `p` x_min       = group' xs x x_max ((x :) . s) 
	| otherwise         = s [] : group' xs x x (x :) 

-- This one works forwards *and* backwards, as well as also being
-- faster that the one in Util.lhs.

{- ORIG:
group p [] = [[]]
group p (x:xs) =
   let ((h1:t1):tt1) = group p xs
       (t,tt) = if null xs then ([],[]) else
		if x `p` h1 then (h1:t1,tt1) else
		   ([], (h1:t1):tt1)
   in ((x:t):tt)
-}

generalMerge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
generalMerge p xs [] = xs
generalMerge p [] ys = ys
generalMerge p (x:xs) (y:ys) | x `p` y   = x : generalMerge p xs (y:ys)
			     | otherwise = y : generalMerge p (x:xs) ys

-- gamma is now called balancedFold

balancedFold :: (a -> a -> a) -> [a] -> a
balancedFold f [] = error "can't reduce an empty list using balancedFold"
balancedFold f [x] = x
balancedFold f l  = balancedFold f (balancedFold' f l)

balancedFold' :: (a -> a -> a) -> [a] -> [a]
balancedFold' f (x:y:xs) = f x y : balancedFold' f xs
balancedFold' f xs = xs

generalMergeSort p [] = []
generalMergeSort p xs = (balancedFold (generalMerge p) . map (: [])) xs

generalNaturalMergeSort p [] = []
generalNaturalMergeSort p xs = (balancedFold (generalMerge p) . group p) xs

#if NOT_USED
mergeSort, naturalMergeSort :: Ord a => [a] -> [a]

mergeSort = generalMergeSort (<=)
naturalMergeSort = generalNaturalMergeSort (<=)

mergeSortLe le = generalMergeSort le
#endif

naturalMergeSortLe le = generalNaturalMergeSort le
\end{code}

%************************************************************************
%*									*
\subsection[Utils-transitive-closure]{Transitive closure}
%*									*
%************************************************************************

This algorithm for transitive closure is straightforward, albeit quadratic.

\begin{code}
transitiveClosure :: (a -> [a])		-- Successor function
		  -> (a -> a -> Bool)	-- Equality predicate
		  -> [a]
		  -> [a]		-- The transitive closure

transitiveClosure succ eq xs
 = go [] xs
 where
   go done [] 			   = done
   go done (x:xs) | x `is_in` done = go done xs
   		  | otherwise      = go (x:done) (succ x ++ xs)

   x `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
  		    | otherwise = x `is_in` ys
\end{code}

%************************************************************************
%*									*
\subsection[Utils-accum]{Accumulating}
%*									*
%************************************************************************

@mapAccumL@ behaves like a combination
of  @map@ and @foldl@;
it applies a function to each element of a list, passing an accumulating
parameter from left to right, and returning a final value of this
accumulator together with the new list.

\begin{code}
mapAccumL :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
					-- and accumulator, returning new
					-- accumulator and elt of result list
	    -> acc 		-- Initial accumulator
	    -> [x] 		-- Input list
	    -> (acc, [y])		-- Final accumulator and result list

mapAccumL f b []     = (b, [])
mapAccumL f b (x:xs) = (b'', x':xs') where
					  (b', x') = f b x
					  (b'', xs') = mapAccumL f b' xs
\end{code}

@mapAccumR@ does the same, but working from right to left instead.  Its type is
the same as @mapAccumL@, though.

\begin{code}
mapAccumR :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
					-- and accumulator, returning new
					-- accumulator and elt of result list
	    -> acc 		-- Initial accumulator
	    -> [x] 		-- Input list
	    -> (acc, [y])		-- Final accumulator and result list

mapAccumR f b []     = (b, [])
mapAccumR f b (x:xs) = (b'', x':xs') where
					  (b'', x') = f b' x
					  (b', xs') = mapAccumR f b xs
\end{code}

Here is the bi-directional version, that works from both left and right.

\begin{code}
mapAccumB :: (accl -> accr -> x -> (accl, accr,y))
      				-- Function of elt of input list
      				-- and accumulator, returning new
      				-- accumulator and elt of result list
	  -> accl 			-- Initial accumulator from left
	  -> accr 			-- Initial accumulator from right
	  -> [x] 			-- Input list
	  -> (accl, accr, [y])	-- Final accumulators and result list

mapAccumB f a b []     = (a,b,[])
mapAccumB f a b (x:xs) = (a'',b'',y:ys)
   where
	(a',b'',y)  = f a b' x
	(a'',b',ys) = mapAccumB f a' b xs
\end{code}

A strict version of foldl.

\begin{code}
foldl'        :: (a -> b -> a) -> a -> [b] -> a
foldl' f z xs = lgo z xs
	     where
		lgo z []     =  z
		lgo z (x:xs) = (lgo $! (f z x)) xs
\end{code}

A combination of foldl with zip.  It works with equal length lists.

\begin{code}
foldl2 :: (acc -> a -> b -> acc) -> acc -> [a] -> [b] -> acc
foldl2 k z [] [] = z
foldl2 k z (a:as) (b:bs) = foldl2 k (k z a b) as bs
\end{code}

Count the number of times a predicate is true

\begin{code}
count :: (a -> Bool) -> [a] -> Int
count p [] = 0
count p (x:xs) | p x       = 1 + count p xs
	       | otherwise = count p xs
\end{code}

@splitAt@, @take@, and @drop@ but with length of another
list giving the break-off point:

\begin{code}
takeList :: [b] -> [a] -> [a]
takeList [] _ = []
takeList (_:xs) ls = 
   case ls of
     [] -> []
     (y:ys) -> y : takeList xs ys

dropList :: [b] -> [a] -> [a]
dropList [] xs    = xs
dropList _  xs@[] = xs
dropList (_:xs) (_:ys) = dropList xs ys


splitAtList :: [b] -> [a] -> ([a], [a])
splitAtList [] xs     = ([], xs)
splitAtList _ xs@[]   = (xs, xs)
splitAtList (_:xs) (y:ys) = (y:ys', ys'')
    where
      (ys', ys'') = splitAtList xs ys

\end{code}


%************************************************************************
%*									*
\subsection[Utils-comparison]{Comparisons}
%*									*
%************************************************************************

\begin{code}
eqListBy :: (a->a->Bool) -> [a] -> [a] -> Bool
eqListBy eq []     []     = True
eqListBy eq (x:xs) (y:ys) = eq x y && eqListBy eq xs ys
eqListBy eq xs     ys     = False

equalLength :: [a] -> [b] -> Bool
equalLength [] []         = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength xs    ys      = False

compareLength :: [a] -> [b] -> Ordering
compareLength [] []         = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength [] _ys        = LT
compareLength _xs []        = GT

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ   any = any
thenCmp other any = other

cmpList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
    -- `cmpList' uses a user-specified comparer

cmpList cmp []     [] = EQ
cmpList cmp []     _  = LT
cmpList cmp _      [] = GT
cmpList cmp (a:as) (b:bs)
  = case cmp a b of { EQ -> cmpList cmp as bs; xxx -> xxx }
\end{code}

\begin{code}
prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] _str = True
prefixMatch _pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
			  | otherwise = False

maybePrefixMatch :: String -> String -> Maybe String
maybePrefixMatch []    rest = Just rest
maybePrefixMatch (_:_) []   = Nothing
maybePrefixMatch (p:pat) (r:rest)
  | p == r    = maybePrefixMatch pat rest
  | otherwise = Nothing

suffixMatch :: Eq a => [a] -> [a] -> Bool
suffixMatch pat str = prefixMatch (reverse pat) (reverse str)
\end{code}

%************************************************************************
%*									*
\subsection[Utils-pairs]{Pairs}
%*									*
%************************************************************************

The following are curried versions of @fst@ and @snd@.

\begin{code}
#if NOT_USED
cfst :: a -> b -> a	-- stranal-sem only (Note)
cfst x y = x
#endif
\end{code}

The following provide us higher order functions that, when applied
to a function, operate on pairs.

\begin{code}
#if NOT_USED
applyToPair :: ((a -> c),(b -> d)) -> (a,b) -> (c,d)
applyToPair (f,g) (x,y) = (f x, g y)

applyToFst :: (a -> c) -> (a,b)-> (c,b)
applyToFst f (x,y) = (f x,y)

applyToSnd :: (b -> d) -> (a,b) -> (a,d)
applyToSnd f (x,y) = (x,f y)
#endif

foldPair :: (a->a->a,b->b->b) -> (a,b) -> [(a,b)] -> (a,b)
foldPair fg ab [] = ab
foldPair fg@(f,g) ab ((a,b):abs) = (f a u,g b v)
		       where (u,v) = foldPair fg ab abs
\end{code}

\begin{code}
unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs
\end{code}

\begin{code}
seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = x `seq` seqList xs b
\end{code}

Global variables:

\begin{code}
global :: a -> IORef a
global a = unsafePerformIO (newIORef a)
\end{code}

Module names:

\begin{code}
looksLikeModuleName [] = False
looksLikeModuleName (c:cs) = isUpper c && go cs
  where go [] = True
	go ('.':cs) = looksLikeModuleName cs
	go (c:cs)   = (isAlphaNum c || c == '_') && go cs
\end{code}

Akin to @Prelude.words@, but sensitive to dquoted entities treating
them as single words.

\begin{code}
toArgs :: String -> [String]
toArgs "" = []
toArgs s  =
  case break (\ ch -> isSpace ch || ch == '"') (dropWhile isSpace s) of -- "
    (w,aft) ->
       (\ ws -> if null w then ws else w : ws) $
       case aft of
	 []           -> []
	 (x:xs)
	   | x /= '"'  -> toArgs xs
	   | otherwise ->
             case lex aft of
	       ((str,rs):_) -> stripQuotes str : toArgs rs
	       _            -> [aft]
 where
    -- strip away dquotes; assume first and last chars contain quotes.
   stripQuotes :: String -> String
   stripQuotes ('"':xs)  = init xs
   stripQuotes xs        = xs
\end{code}
