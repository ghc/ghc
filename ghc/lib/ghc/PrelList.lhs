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

import {-# SOURCE #-}	IOBase	( error )
import PrelTup
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
head []                 =  error "PreludeList.head: empty list"

last                    :: [a] -> a
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  error "PreludeList.last: empty list"

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  error "PreludeList.tail: empty list"

init                    :: [a] -> [a]
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  error "PreludeList.init: empty list"

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.
length                  :: [a] -> Int
length []               =  0
length (_:l)            =  1 + length l

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
foldl1 _ []             =  error "PreludeList.foldl1: empty list"

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs            =  q : (case xs of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  error "PreludeList.scanl1: empty list"

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 f [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  error "PreludeList.foldr1: empty list"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs 

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]           =  [x]
scanr1 f  (x:xs)        =  f x q : qs
                           where qs@(q:_) = scanr1 f xs 
scanr1 _ []             =  error "PreludeList.scanr1: empty list"

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

take                   :: Int -> [a] -> [a]
take 0 _               =  []
take _ []              =  []
take n (x:xs) | n > 0  =  x : take (n-1) xs
take _     _           =  error "PreludeList.take: negative argument"

drop                   :: Int -> [a] -> [a]
drop 0 xs              =  xs
drop _ []              =  []
drop n (_:xs) | n > 0  =  drop (n-1) xs
drop _     _           =  error "PreludeList.drop: negative argument"

splitAt                   :: Int -> [a] -> ([a],[a])
splitAt 0 xs              =  ([],xs)
splitAt _ []              =  ([],[])
splitAt n (x:xs) | n > 0  =  (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs
splitAt _     _           =  error "PreludeList.splitAt: negative argument"

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p []               =  ([],[])
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)
break p                 =  span (not . p)

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
reverse                 :: [a] -> [a]
reverse                 =  foldl (flip (:)) []

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or                 :: [Bool] -> Bool
and                     =  foldr (&&) True
or                      =  foldr (||) False

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all                :: (a -> Bool) -> [a] -> Bool
any p                   =  or . map p
all p                   =  and . map p

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
elem, notElem           :: (Eq a) => a -> [a] -> Bool
elem x                  =  any (== x)
notElem x               =  all (/= x)

-- lookup key assocs looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []           =  Nothing
lookup key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- sum and product compute the sum or product of a finite list of numbers.
sum, product            :: (Num a) => [a] -> a
sum                     =  foldl (+) 0  
product                 =  foldl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
maximum, minimum        :: (Ord a) => [a] -> a
maximum []              =  error "PreludeList.maximum: empty list"
maximum xs              =  foldl1 max xs

minimum []              =  error "PreludeList.minimum: empty list"
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
zip                     =  zipWith (,)

zip3                    :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3                    =  zipWith3 (,,)

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
unlines			=  concatMap (++ "\n")

unwords			:: [String] -> String
unwords []		=  ""
unwords ws		=  foldr1 (\w s -> w ++ ' ':s) ws
\end{code}
