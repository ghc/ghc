%
% (c) The University of Glasgow 2006
% (c) The University of Glasgow 1992-2002
%
\section[Util]{Highly random utility functions}

\begin{code}
module Util (

	-- general list processing
	zipEqual, zipWithEqual, zipWith3Equal, zipWith4Equal,
        zipLazy, stretchZipWith,
	mapFst, mapSnd,
	mapAndUnzip, mapAndUnzip3,
	nOfThem, filterOut, partitionWith, splitEithers,

	lengthExceeds, lengthIs, lengthAtLeast, 
	listLengthCmp, atLength, equalLength, compareLength,

	isSingleton, only, singleton,
	notNull, snocView,

	isIn, isn'tIn,

	-- for-loop
	nTimes,

	-- sorting
	sortLe, sortWith,

	-- transitive closures
	transitiveClosure,

	-- accumulating
	mapAccumL, mapAccumR, mapAccumB, 
	foldl2, count, all2,
	
	takeList, dropList, splitAtList, split,

	-- comparisons
	isEqual, eqListBy, 
	thenCmp, cmpList, prefixMatch, suffixMatch, maybePrefixMatch,
	removeSpaces,

	-- strictness
	foldl', seqList,

	-- pairs
	unzipWith,

	global, consIORef,

	-- module names
	looksLikeModuleName,
	
	toArgs,

	-- Floating point stuff
	readRational,

	-- IO-ish utilities
	createDirectoryHierarchy,
	doesDirNameExist,
	modificationTimeIfExists,

	later, handleDyn, handle,

	-- Filename utils
	Suffix,
	splitFilename, suffixOf, basenameOf, joinFileExt,
	splitFilenameDir, joinFileName,
	splitFilename3,
	splitLongestPrefix,
	replaceFilenameSuffix, directoryOf, filenameOf,
	replaceFilenameDirectory,
	escapeSpaces, isPathSeparator,
	parseSearchPath,
	normalisePath, platformPath, pgmPath,
    ) where

#include "HsVersions.h"

import Panic		( panic, trace )
import FastTypes

import Control.Exception ( Exception(..), finally, catchDyn, throw )
import qualified Control.Exception as Exception
import Data.Dynamic	( Typeable )
import Data.IORef	( IORef, newIORef )
import System.IO.Unsafe	( unsafePerformIO )
import Data.IORef	( readIORef, writeIORef )

import qualified Data.List as List ( elem, notElem )

#ifndef DEBUG
import Data.List		( zipWith4 )
#endif

import Control.Monad	( when )
import SYSTEM_IO_ERROR as IO ( catch, isDoesNotExistError )
import System.Directory	( doesDirectoryExist, createDirectory,
                          getModificationTime )
import Data.Char	( isUpper, isAlphaNum, isSpace, ord, isDigit )
import Data.Ratio	( (%) )
import System.Time	( ClockTime )

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

\begin{code}
filterOut :: (a->Bool) -> [a] -> [a]
-- Like filter, only reverses the sense of the test
filterOut p [] = []
filterOut p (x:xs) | p x       = filterOut p xs
		   | otherwise = x : filterOut p xs

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f [] = ([],[])
partitionWith f (x:xs) = case f x of
			   Left  b -> (b:bs, cs)
			   Right c -> (bs, c:cs)
		       where
			 (bs,cs) = partitionWith f xs

splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([],[])
splitEithers (e : es) = case e of
			  Left x -> (x:xs, ys)
			  Right y -> (xs, y:ys)
		      where
			(xs,ys) = splitEithers es
\end{code}

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
mapFst :: (a->c) -> [(a,b)] -> [(c,b)]
mapSnd :: (b->c) -> [(a,b)] -> [(a,c)]

mapFst f xys = [(f x, y) | (x,y) <- xys]
mapSnd f xys = [(x, f y) | (x,y) <- xys]

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

equalLength :: [a] -> [b] -> Bool
equalLength [] []         = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength xs    ys      = False

compareLength :: [a] -> [b] -> Ordering
compareLength [] []         = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength [] _ys        = LT
compareLength _xs []        = GT

----------------------------
singleton :: a -> [a]
singleton x = [x]

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton  _  = False

notNull :: [a] -> Bool
notNull [] = False
notNull _  = True

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
-- Given a <= function, group finds maximal contiguous up-runs 
-- or down-runs in the input list.
-- It's stable, in the sense that it never re-orders equal elements
--
-- Date: Mon, 12 Feb 1996 15:09:41 +0000
-- From: Andy Gill <andy@dcs.gla.ac.uk>
-- Here is a `better' definition of group.

group p []     = []
group p (x:xs) = group' xs x x (x :)
  where
    group' []     _     _     s  = [s []]
    group' (x:xs) x_min x_max s 
	|      x_max `p` x  = group' xs x_min x (s . (x :)) 
	| not (x_min `p` x) = group' xs x x_max ((x :) . s) 
	| otherwise         = s [] : group' xs x x (x :) 
	-- NB: the 'not' is essential for stablity
	--	x `p` x_min would reverse equal elements

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

generalNaturalMergeSort p [] = []
generalNaturalMergeSort p xs = (balancedFold (generalMerge p) . group p) xs

#if NOT_USED
generalMergeSort p [] = []
generalMergeSort p xs = (balancedFold (generalMerge p) . map (: [])) xs

mergeSort, naturalMergeSort :: Ord a => [a] -> [a]

mergeSort = generalMergeSort (<=)
naturalMergeSort = generalNaturalMergeSort (<=)

mergeSortLe le = generalMergeSort le
#endif

sortLe :: (a->a->Bool) -> [a] -> [a]
sortLe le = generalNaturalMergeSort le

sortWith :: Ord b => (a->b) -> [a] -> [a]
sortWith get_key xs = sortLe le xs
  where
    x `le` y = get_key x < get_key y	
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

all2 :: (a -> b -> Bool) -> [a] -> [b] -> Bool
-- True if the lists are the same length, and 
-- all corresponding elements satisfy the predicate
all2 p []     []     = True
all2 p (x:xs) (y:ys) = p x y && all2 p xs ys
all2 p xs     ys     = False
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

snocView :: [a] -> Maybe ([a],a)
	-- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
	    where
		-- Invariant: second arg is non-empty
	      go acc [x]    = Just (reverse acc, x)
	      go acc (x:xs) = go (x:acc) xs

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s
\end{code}


%************************************************************************
%*									*
\subsection[Utils-comparison]{Comparisons}
%*									*
%************************************************************************

\begin{code}
isEqual :: Ordering -> Bool
-- Often used in (isEqual (a `compare` b))
isEqual GT = False
isEqual EQ = True
isEqual LT = False

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ   any = any
thenCmp other any = other

eqListBy :: (a->a->Bool) -> [a] -> [a] -> Bool
eqListBy eq []     []     = True
eqListBy eq (x:xs) (y:ys) = eq x y && eqListBy eq xs ys
eqListBy eq xs     ys     = False

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

removeSpaces :: String -> String
removeSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
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

\begin{code}
consIORef :: IORef [a] -> a -> IO ()
consIORef var x = do
  xs <- readIORef var
  writeIORef var (x:xs)
\end{code}

Module names:

\begin{code}
looksLikeModuleName [] = False
looksLikeModuleName (c:cs) = isUpper c && go cs
  where go [] = True
	go ('.':cs) = looksLikeModuleName cs
	go (c:cs)   = (isAlphaNum c || c == '_') && go cs
\end{code}

Akin to @Prelude.words@, but acts like the Bourne shell, treating
quoted strings and escaped characters within the input as solid blocks
of characters.  Doesn't raise any exceptions on malformed escapes or
quoting.

\begin{code}
toArgs :: String -> [String]
toArgs "" = []
toArgs s  =
  case dropWhile isSpace s of  -- drop initial spacing
    [] -> []  -- empty, so no more tokens
    rem -> let (tok,aft) = token rem [] in tok : toArgs aft
 where
   -- Grab a token off the string, given that the first character exists and
   -- isn't whitespace.  The second argument is an accumulator which has to be
   -- reversed at the end.
  token [] acc = (reverse acc,[])            -- out of characters
  token ('\\':c:aft) acc                     -- escapes
               = token aft ((escape c) : acc)
  token (q:aft) acc | q == '"' || q == '\''  -- open quotes
               = let (aft',acc') = quote q aft acc in token aft' acc'
  token (c:aft) acc | isSpace c              -- unescaped, unquoted spacing
               = (reverse acc,aft)
  token (c:aft) acc                          -- anything else goes in the token
               = token aft (c:acc)

   -- Get the appropriate character for a single-character escape.
  escape 'n' = '\n'
  escape 't' = '\t'
  escape 'r' = '\r'
  escape c   = c

   -- Read into accumulator until a quote character is found.
  quote qc =
    let quote' [] acc                  = ([],acc)
        quote' ('\\':c:aft) acc        = quote' aft ((escape c) : acc)
        quote' (c:aft) acc | c == qc   = (aft,acc)
        quote' (c:aft) acc             = quote' aft (c:acc)
    in quote'
\end{code}

-- -----------------------------------------------------------------------------
-- Floats

\begin{code}
readRational__ :: ReadS Rational -- NB: doesn't handle leading "-"
readRational__ r = do 
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r = do
	(ds,s)  <- lexDecDigits r
	(ds',t) <- lexDotDigits s
	return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s			   = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do
			(k,t) <- readDec s
			return (-k,t)
     readExp' s	      = readDec s

     readDec s = do
        (ds,r) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)

readRational :: String -> Rational -- NB: *does* handle a leading "-"
readRational top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,"") <- readRational__ s ; return x }) of
	  [x] -> x
	  []  -> error ("readRational: no parse:"        ++ top_s)
	  _   -> error ("readRational: ambiguous parse:" ++ top_s)


-----------------------------------------------------------------------------
-- Create a hierarchy of directories

createDirectoryHierarchy :: FilePath -> IO ()
createDirectoryHierarchy dir = do
  b <- doesDirectoryExist dir
  when (not b) $ do
	createDirectoryHierarchy (directoryOf dir)
	createDirectory dir

-----------------------------------------------------------------------------
-- Verify that the 'dirname' portion of a FilePath exists.
-- 
doesDirNameExist :: FilePath -> IO Bool
doesDirNameExist fpath = doesDirectoryExist (directoryOf fpath)

-- -----------------------------------------------------------------------------
-- Exception utils

later = flip finally

handleDyn :: Typeable ex => (ex -> IO a) -> IO a -> IO a
handleDyn = flip catchDyn

handle :: (Exception -> IO a) -> IO a -> IO a
#if __GLASGOW_HASKELL__ < 501
handle = flip Exception.catchAllIO
#else
handle h f = f `Exception.catch` \e -> case e of
    ExitException _ -> throw e
    _               -> h e
#endif

-- --------------------------------------------------------------
-- check existence & modification time at the same time

modificationTimeIfExists :: FilePath -> IO (Maybe ClockTime)
modificationTimeIfExists f = do
  (do t <- getModificationTime f; return (Just t))
	`IO.catch` \e -> if isDoesNotExistError e 
			then return Nothing 
			else ioError e

-- --------------------------------------------------------------
-- Filename manipulation
		
-- Filenames are kept "normalised" inside GHC, using '/' as the path
-- separator.  On Windows these functions will also recognise '\\' as
-- the path separator, but will generally construct paths using '/'.

type Suffix = String

splitFilename :: String -> (String,Suffix)
splitFilename f = splitLongestPrefix f (=='.')

basenameOf :: FilePath -> String
basenameOf = fst . splitFilename

suffixOf :: FilePath -> Suffix
suffixOf = snd . splitFilename

joinFileExt :: String -> String -> FilePath
joinFileExt path ""  = path
joinFileExt path ext = path ++ '.':ext

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitFilenameDir :: String -> (String,String)
splitFilenameDir str
   = let (dir, rest) = splitLongestPrefix str isPathSeparator
	 (dir', rest') | null rest = (".", dir)
		       | otherwise = (dir, rest)
     in  (dir', rest')

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", ".ext")
splitFilename3 :: String -> (String,String,Suffix)
splitFilename3 str
   = let (dir, rest) = splitFilenameDir str
	 (name, ext) = splitFilename rest
     in  (dir, name, ext)

joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname = dir ++ '/':fname

-- split a string at the last character where 'pred' is True,
-- returning a pair of strings. The first component holds the string
-- up (but not including) the last character for which 'pred' returned
-- True, the second whatever comes after (but also not including the
-- last character).
--
-- If 'pred' returns False for all characters in the string, the original
-- string is returned in the first component (and the second one is just
-- empty).
splitLongestPrefix :: String -> (Char -> Bool) -> (String,String)
splitLongestPrefix str pred
  | null r_pre = (str,           [])
  | otherwise  = (reverse (tail r_pre), reverse r_suf)
	-- 'tail' drops the char satisfying 'pred'
  where 
    (r_suf, r_pre) = break pred (reverse str)

replaceFilenameSuffix :: FilePath -> Suffix -> FilePath
replaceFilenameSuffix file suf = basenameOf file `joinFileExt` suf

-- directoryOf strips the filename off the input string, returning
-- the directory.
directoryOf :: FilePath -> String
directoryOf = fst . splitFilenameDir

-- filenameOf strips the directory off the input string, returning
-- the filename.
filenameOf :: FilePath -> String
filenameOf = snd . splitFilenameDir

replaceFilenameDirectory :: FilePath -> String -> FilePath
replaceFilenameDirectory path dir = dir `joinFileName` filenameOf path

escapeSpaces :: String -> String
escapeSpaces = foldr (\c s -> if isSpace c then '\\':c:s else c:s) ""

isPathSeparator :: Char -> Bool
isPathSeparator ch =
#ifdef mingw32_TARGET_OS
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif

--------------------------------------------------------------
-- * Search path
--------------------------------------------------------------

-- | The function splits the given string to substrings
-- using the 'searchPathSeparator'.
parseSearchPath :: String -> [FilePath]
parseSearchPath path = split path
  where
    split :: String -> [String]
    split s =
      case rest' of
        []     -> [chunk] 
        _:rest -> chunk : split rest
      where
        chunk = 
          case chunk' of
#ifdef mingw32_HOST_OS
            ('\"':xs@(_:_)) | last xs == '\"' -> init xs
#endif
            _                                 -> chunk'

        (chunk', rest') = break (==searchPathSeparator) s

-- | A platform-specific character used to separate search path strings in 
-- environment variables. The separator is a colon (\":\") on Unix and Macintosh, 
-- and a semicolon (\";\") on the Windows operating system.
searchPathSeparator :: Char
#if mingw32_HOST_OS || mingw32_TARGET_OS
searchPathSeparator = ';'
#else
searchPathSeparator = ':'
#endif

-----------------------------------------------------------------------------
-- Convert filepath into platform / MSDOS form.

-- We maintain path names in Unix form ('/'-separated) right until 
-- the last moment.  On Windows we dos-ify them just before passing them
-- to the Windows command.
-- 
-- The alternative, of using '/' consistently on Unix and '\' on Windows,
-- proved quite awkward.  There were a lot more calls to platformPath,
-- and even on Windows we might invoke a unix-like utility (eg 'sh'), which
-- interpreted a command line 'foo\baz' as 'foobaz'.

normalisePath :: String -> String
-- Just changes '\' to '/'

pgmPath :: String		-- Directory string in Unix format
	-> String		-- Program name with no directory separators
				--	(e.g. copy /y)
	-> String		-- Program invocation string in native format

#if defined(mingw32_HOST_OS)
--------------------- Windows version ------------------
normalisePath xs = subst '\\' '/' xs
pgmPath dir pgm  = platformPath dir ++ '\\' : pgm
platformPath p   = subst '/' '\\' p

subst a b ls = map (\ x -> if x == a then b else x) ls
#else
--------------------- Non-Windows version --------------
normalisePath xs   = xs
pgmPath dir pgm    = dir ++ '/' : pgm
platformPath stuff = stuff
--------------------------------------------------------
#endif
\end{code}
