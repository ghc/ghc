%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelList]{Module @PrelList@}

The List data type and its operations

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelList (
   [] (..),

   head, last, tail, init, null, length, (!!),
   foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
   iterate, repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break,
   lines, words, unlines, unwords, reverse, and, or,
   any, all, elem, notElem, lookup,
   sum, product, maximum, minimum, concatMap, 
   zip, zip3, zipWith, zipWith3, unzip, unzip3
 ) where

import {-# SOURCE #-} PrelErr ( error )
import PrelTup
import PrelMaybe
import PrelBase

infix  4 `elem`, `notElem`
\end{code}

%*********************************************************
%*							*
\subsection{List-manipulation functions}
%*							*
%*********************************************************

\begin{code}
-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.

head                    :: [a] -> a
head (x:_)              =  x
head []                 =  errorEmptyList "head"

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

last                    :: [a] -> a
#ifdef USE_REPORT_PRELUDE
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  errorEmptyList "last"
#else
-- eliminate repeated cases
last []     		=  errorEmptyList "last"
last (x:xs) 		=  last' x xs
  where last' x []     = x
	last' _ (x:xs) = last' x xs
#endif

init                    :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  errorEmptyList "init"
#else
-- eliminate repeated cases
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' x []     = []
	init' x (y:xs) = x : init' y xs
#endif

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.
length                  :: [a] -> Int
#ifdef USE_REPORT_PRELUDE
length []               =  0
length (_:l)            =  1 + length l
#else
length l                =  len l 0#
  where
    len :: [a] -> Int# -> Int
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)
#endif

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

foldl                   :: (a -> b -> a) -> a -> [b] -> a
foldl f z []            =  z
foldl f z (x:xs)        =  foldl f (f z x) xs

foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs            =  q : (case xs of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  errorEmptyList "scanl1"

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 f [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs 

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]           =  [x]
scanr1 f  (x:xs)        =  f x q : qs
                           where qs@(q:_) = scanr1 f xs 
scanr1 _ []             =  errorEmptyList "scanr1"

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
iterate                 :: (a -> a) -> a -> [a]
iterate f x             =  x : iterate f (f x)

-- repeat x is an infinite list, with x the value of every element.
repeat                  :: a -> [a]
repeat x                =  xs where xs = x:xs

-- replicate n x is a list of length n with x the value of every element
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle                   :: [a] -> [a]
cycle xs                =  xs' where xs' = xs ++ xs'

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).
#ifdef USE_REPORT_PRELUDE
take                   :: Int -> [a] -> [a]
take 0 _               =  []
take _ []              =  []
take n (x:xs) | n > 0  =  x : take (n-1) xs
take _     _           =  errorNegativeIdx "take"

drop                   :: Int -> [a] -> [a]
drop 0 xs              =  xs
drop _ []              =  []
drop n (_:xs) | n > 0  =  drop (n-1) xs
drop _     _           =  errorNegativeIdx "drop"

splitAt                   :: Int -> [a] -> ([a],[a])
splitAt 0 xs              =  ([],xs)
splitAt _ []              =  ([],[])
splitAt n (x:xs) | n > 0  =  (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs
splitAt _     _           =  errorNegativeIdx "splitAt"

#else /* hack away */
take	:: Int -> [b] -> [b]
take (I# n#) xs = takeUInt n# xs

-- The general code for take, below, checks n <= maxInt
-- No need to check for maxInt overflow when specialised
-- at type Int or Int# since the Int must be <= maxInt

takeUInt :: Int# -> [b] -> [b]
takeUInt n xs
  | n >=# 0#  =  take_unsafe_UInt n xs
  | otherwise =  errorNegativeIdx "take"

take_unsafe_UInt 0# _     = []
take_unsafe_UInt m  ls    =
  case ls of
    []     -> ls
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

drop		:: Int -> [b] -> [b]
drop (I# n#) xs
  | n# <# 0#	= errorNegativeIdx "drop"
  | otherwise	= drop# n# xs
    where
	drop# :: Int# -> [a] -> [a]
	drop# 0# xs      = xs
	drop# _  xs@[]	 = xs
	drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt	:: Int -> [b] -> ([b], [b])
splitAt (I# n#) xs
  | n# <# 0#	= errorNegativeIdx "splitAt"
  | otherwise	= splitAt# n# xs
    where
	splitAt# :: Int# -> [a] -> ([a], [a])
	splitAt# 0# xs	   = ([], xs)
	splitAt# _  xs@[]  = (xs, xs)
	splitAt# m# (x:xs) = (x:xs', xs'')
	  where
	    (xs', xs'') = splitAt# (m# -# 1#) xs

#endif /* USE_REPORT_PRELUDE */

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

#ifdef USE_REPORT_PRELUDE
break p                 =  span (not . p)
#else
-- HBC version (stolen)
break p xs@[]		=  (xs, xs)
break p xs@(x:xs')
	   | p x	=  ([],xs)
	   | otherwise	=  let (ys,zs) = break p xs' in (x:ys,zs)
#endif

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
reverse                 :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
reverse                 =  foldl (flip (:)) []
#else
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
#endif

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or                 :: [Bool] -> Bool
#ifdef USE_REPORT_PRELUDE
and                     =  foldr (&&) True
or                      =  foldr (||) False
#else
and []		=  True
and (x:xs)	=  x && and xs
or []		=  False
or (x:xs)	=  x || or xs
#endif

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all                :: (a -> Bool) -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
any p                   =  or . map p
all p                   =  and . map p
#else
any p []	= False
any p (x:xs)	= p x || any p xs
all p []	=  True
all p (x:xs)	=  p x && all p xs
#endif

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
elem, notElem           :: (Eq a) => a -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
elem x                  =  any (== x)
notElem x               =  all (/= x)
#else
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys

notElem	x []	=  True
notElem x (y:ys)=  x /= y && notElem x ys
#endif

-- lookup key assocs looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []           =  Nothing
lookup key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- sum and product compute the sum or product of a finite list of numbers.
{-# SPECIALISE sum     :: [Int] -> Int #-}
{-# SPECIALISE product :: [Int] -> Int #-}
sum, product            :: (Num a) => [a] -> a
#ifdef USE_REPORT_PRELUDE
sum                     =  foldl (+) 0  
product                 =  foldl (*) 1
#else
sum	l	= sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
product	l	= prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
#endif

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
{-# SPECIALISE maximum :: [Int] -> Int #-}
{-# SPECIALISE minimum :: [Int] -> Int #-}
maximum, minimum        :: (Ord a) => [a] -> a
maximum []              =  errorEmptyList "maximum"
maximum xs              =  foldl1 max xs

minimum []              =  errorEmptyList "minimum"
minimum xs              =  foldl1 min xs

concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []
\end{code}


%*********************************************************
%*							*
\subsection{The zip family}
%*							*
%*********************************************************

zip takes two lists and returns a list of corresponding pairs.  If one
input list is short, excess elements of the longer list are discarded.
zip3 takes three lists and returns a list of triples.  Zips for larger
tuples are in the List library

\begin{code}
zip                     :: [a] -> [b] -> [(a,b)]
-- Specification
-- zip =  zipWith (,)
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

zip3                    :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.

zipWith                 :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith _ _ _           =  []

zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []


-- unzip transforms a list of pairs into a pair of lists.  

unzip                   :: [(a,b)] -> ([a],[b])
unzip                   =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3                  :: [(a,b,c)] -> ([a],[b],[c])
unzip3                  =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                                 ([],[],[])
\end{code}

%*********************************************************
%*							*
\subsection{Functions on strings}
%*							*
%*********************************************************

lines breaks a string up into a list of strings at newline characters.
The resulting strings do not contain newlines.  Similary, words
breaks a string up into a list of words, which were delimited by
white space.  unlines and unwords are the inverse operations.
unlines joins lines with terminating newlines, and unwords joins
words with separating spaces.

\begin{code}
lines			:: String -> [String]
lines ""		=  []
lines s			=  let (l, s') = break (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> lines s''

words			:: String -> [String]
words s			=  case dropWhile {-partain:Char.-}isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = 
                                             break {-partain:Char.-}isSpace s'

unlines			:: [String] -> String
#ifdef USE_REPORT_PRELUDE
unlines			=  concatMap (++ "\n")
#else
-- HBC version (stolen)
-- here's a more efficient version
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
#endif

unwords			:: [String] -> String
#ifdef USE_REPORT_PRELUDE
unwords []		=  ""
unwords ws		=  foldr1 (\w s -> w ++ ' ':s) ws
#else
-- HBC version (stolen)
-- here's a more efficient version
unwords []		=  ""
unwords [w]		= w
unwords (w:ws)		= w ++ ' ' : unwords ws
#endif

\end{code}

Common up near identical calls to `error' to reduce the number
constant strings created when compiled:

\begin{code}
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

errorNegativeIdx fun =
 error (prel_list_str ++ fun ++ ": negative index")

prel_list_str = "PreludeList."
\end{code}
