% ------------------------------------------------------------------------------
% $Id: List.lhs,v 1.2 2001/07/03 11:37:50 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[GHC.List]{Module @GHC.List@}

The List data type and its operations

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.List (
   [] (..),

   map, (++), filter, concat,
   head, last, tail, init, null, length, (!!), 
   foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
   iterate, repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break,
   reverse, and, or,
   any, all, elem, notElem, lookup,
   maximum, minimum, concatMap,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
#ifdef USE_REPORT_PRELUDE

#else

   -- non-standard, but hidden when creating the Prelude
   -- export list.
   takeUInt_append

#endif

 ) where

import {-# SOURCE #-} GHC.Err ( error )
import Data.Tuple
import GHC.Maybe
import GHC.Base

infixl 9  !!
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
head []                 =  badHead

badHead = errorEmptyList "head"

-- This rule is useful in cases like 
--	head [y | (x,y) <- ps, x==t]
{-# RULES
"head/build"	forall (g::forall b.(Bool->b->b)->b->b) . 
		head (build g) = g (\x _ -> x) badHead
"head/augment"	forall xs (g::forall b. (a->b->b) -> b -> b) . 
		head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

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
  where last' y []     = y
	last' _ (y:ys) = last' y ys
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
  where init' _ []     = []
	init' y (z:zs) = y : init' z zs
#endif

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.
length                  :: [a] -> Int
length l                =  len l 0#
  where
    len :: [a] -> Int# -> Int
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)

-- filter, applied to a predicate and a list, returns the list of those
-- elements that satisfy the predicate; i.e.,
-- filter p xs = [ x | x <- xs, p x]
filter :: (a -> Bool) -> [a] -> [a]
filter = filterList

filterFB c p x r | p x       = x `c` r
		 | otherwise = r

{-# RULES
"filter"	forall p xs.	filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"filterFB"	forall c p q.	filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
"filterList" 	forall p.	foldr (filterFB (:) p) [] = filterList p
 #-}

-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.

filterList :: (a -> Bool) -> [a] -> [a]
filterList _pred []    = []
filterList pred (x:xs)
  | pred x         = x : filterList pred xs
  | otherwise	   = filterList pred xs

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

-- We write foldl as a non-recursive thing, so that it
-- can be inlined, and then (often) strictness-analysed,
-- and hence the classic space leak on foldl (+) 0 xs

foldl        :: (a -> b -> a) -> a -> [b] -> a
foldl f z xs = lgo z xs
	     where
		lgo z []     =  z
		lgo z (x:xs) = lgo (f z x) xs

foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  errorEmptyList "scanl1"

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs 

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _  [x]           =  [x]
scanr1 f  (x:xs)        =  f x q : qs
                           where qs@(q:_) = scanr1 f xs 
scanr1 _ []             =  errorEmptyList "scanr1"

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]
iterate = iterateList

iterateFB c f x = x `c` iterateFB c f (f x)

iterateList f x =  x : iterateList f (f x)

{-# RULES
"iterate"	forall f x.	iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB" 			iterateFB (:) = iterateList
 #-}


-- repeat x is an infinite list, with x the value of every element.
repeat :: a -> [a]
repeat = repeatList

repeatFB c x = xs where xs = x `c` xs
repeatList x = xs where xs = x :   xs

{-# RULES
"repeat"	forall x. repeat x	= build (\c _n -> repeatFB c x)
"repeatFB" 		  repeatFB (:)	= repeatList
 #-}

-- replicate n x is a list of length n with x the value of every element
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle                   :: [a] -> [a]
cycle []		= error "Prelude.cycle: empty list"
cycle xs		= xs' where xs' = xs ++ xs'

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to 
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).
#ifdef USE_REPORT_PRELUDE
take                   :: Int -> [a] -> [a]
take 0 _               =  []
take _ []              =  []
take n (x:xs) | n > 0  =  x : take (minusInt n 1) xs
take _     _           =  errorNegativeIdx "take"

drop                   :: Int -> [a] -> [a]
drop 0 xs              =  xs
drop _ []              =  []
drop n (_:xs) | n > 0  =  drop (minusInt n 1) xs
drop _     _           =  errorNegativeIdx "drop"


splitAt                   :: Int -> [a] -> ([a],[a])
splitAt 0 xs              =  ([],xs)
splitAt _ []              =  ([],[])
splitAt n (x:xs) | n > 0  =  (x:xs',xs'') where (xs',xs'') = splitAt (minusInt n 1) xs
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

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

takeUInt_append :: Int# -> [b] -> [b] -> [b]
takeUInt_append n xs rs
  | n >=# 0#  =  take_unsafe_UInt_append n xs rs
  | otherwise =  errorNegativeIdx "take"

take_unsafe_UInt_append	:: Int# -> [b] -> [b] -> [b]
take_unsafe_UInt_append	0#  _ rs  = rs
take_unsafe_UInt_append	m  ls rs  =
  case ls of
    []     -> rs
    (x:xs) -> x : take_unsafe_UInt_append (m -# 1#) xs rs

drop		:: Int -> [b] -> [b]
drop (I# n#) ls
  | n# <# 0#	= errorNegativeIdx "drop"
  | otherwise	= drop# n# ls
    where
	drop# :: Int# -> [a] -> [a]
	drop# 0# xs      = xs
	drop# _  xs@[]	 = xs
	drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt	:: Int -> [b] -> ([b], [b])
splitAt (I# n#) ls
  | n# <# 0#	= errorNegativeIdx "splitAt"
  | otherwise	= splitAt# n# ls
    where
	splitAt# :: Int# -> [a] -> ([a], [a])
	splitAt# 0# xs	   = ([], xs)
	splitAt# _  xs@[]  = (xs, xs)
	splitAt# m# (x:xs) = (x:xs', xs'')
	  where
	    (xs', xs'') = splitAt# (m# -# 1#) xs

#endif /* USE_REPORT_PRELUDE */

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

#ifdef USE_REPORT_PRELUDE
break p                 =  span (not . p)
#else
-- HBC version (stolen)
break _ xs@[]		=  (xs, xs)
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

{-# RULES
"and/build"	forall (g::forall b.(Bool->b->b)->b->b) . 
		and (build g) = g (&&) True
"or/build"	forall (g::forall b.(Bool->b->b)->b->b) . 
		or (build g) = g (||) False
 #-}
#endif

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all                :: (a -> Bool) -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
any p                   =  or . map p
all p                   =  and . map p
#else
any _ []	= False
any p (x:xs)	= p x || any p xs

all _ []	=  True
all p (x:xs)	=  p x && all p xs
{-# RULES
"any/build"	forall p (g::forall b.(a->b->b)->b->b) . 
		any p (build g) = g ((||) . p) False
"all/build"	forall p (g::forall b.(a->b->b)->b->b) . 
		all p (build g) = g ((&&) . p) True
 #-}
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

notElem	_ []	=  True
notElem x (y:ys)=  x /= y && notElem x ys
#endif

-- lookup key assocs looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys


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

concat :: [[a]] -> [a]
concat = foldr (++) []

{-# RULES
  "concat" forall xs. concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
 #-}
\end{code}


\begin{code}
-- List index (subscript) operator, 0-origin
(!!)                    :: [a] -> Int -> a
#ifdef USE_REPORT_PRELUDE
(x:_)  !! 0             =  x
(_:xs) !! n | n > 0     =  xs !! (minusInt n 1)
(_:_)  !! _             =  error "Prelude.(!!): negative index"
[]     !! _             =  error "Prelude.(!!): index too large"
#else
-- HBC version (stolen), then unboxified
-- The semantics is not quite the same for error conditions
-- in the more efficient version.
--
xs !! (I# n) | n <# 0#   =  error "Prelude.(!!): negative index\n"
	     | otherwise =  sub xs n
                         where
			    sub :: [a] -> Int# -> a
                            sub []     _ = error "Prelude.(!!): index too large\n"
                            sub (y:ys) n = if n ==# 0#
					   then y
					   else sub ys (n -# 1#)
#endif
\end{code}


%*********************************************************
%*							*
\subsection{The zip family}
%*							*
%*********************************************************

\begin{code}
foldr2 _k z [] 	  _ys	 = z
foldr2 _k z _xs   []	 = z
foldr2 k z (x:xs) (y:ys) = k x y (foldr2 k z xs ys)

foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)

foldr2_right _k z  _y _r []     = z
foldr2_right  k _z  y  r (x:xs) = k x y (r xs)

-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
-- foldr2 k z xs ys = foldr (foldr2_right k z) (\_ -> z) ys xs
{-# RULES
"foldr2/left"	forall k z ys (g::forall b.(a->b->b)->b->b) . 
		  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys

"foldr2/right"	forall k z xs (g::forall b.(a->b->b)->b->b) . 
		  foldr2 k z xs (build g) = g (foldr2_right k z) (\_ -> z) xs
 #-}
\end{code}

The foldr2/right rule isn't exactly right, because it changes
the strictness of foldr2 (and thereby zip)

E.g. main = print (null (zip nonobviousNil (build undefined)))
          where   nonobviousNil = f 3
                  f n = if n == 0 then [] else f (n-1)

I'm going to leave it though.


zip takes two lists and returns a list of corresponding pairs.  If one
input list is short, excess elements of the longer list are discarded.
zip3 takes three lists and returns a list of triples.  Zips for larger
tuples are in the List module.

\begin{code}
----------------------------------------------
zip :: [a] -> [b] -> [(a,b)]
zip = zipList

zipFB c x y r = (x,y) `c` r


zipList               :: [a] -> [b] -> [(a,b)]
zipList (a:as) (b:bs) = (a,b) : zipList as bs
zipList _      _      = []

{-# RULES
"zip"		forall xs ys. zip xs ys	= build (\c n -> foldr2 (zipFB c) n xs ys)
"zipList"	foldr2 (zipFB (:)) []   = zipList
 #-}
\end{code}

\begin{code}
----------------------------------------------
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []
\end{code}


-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.


\begin{code}
----------------------------------------------
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith = zipWithList


zipWithFB c f x y r = (x `f` y) `c` r

zipWithList                 :: (a->b->c) -> [a] -> [b] -> [c]
zipWithList f (a:as) (b:bs) = f a b : zipWithList f as bs
zipWithList _ _      _      = []

{-# RULES
"zipWith"	forall f xs ys.	zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
"zipWithList"	forall f. 	foldr2 (zipWithFB (:) f) [] = zipWithList f
  #-}
\end{code}

\begin{code}
zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []

-- unzip transforms a list of pairs into a pair of lists.  
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])
\end{code}


%*********************************************************
%*							*
\subsection{Error code}
%*							*
%*********************************************************

Common up near identical calls to `error' to reduce the number
constant strings created when compiled:

\begin{code}
errorEmptyList :: String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

errorNegativeIdx :: String -> a
errorNegativeIdx fun =
 error (prel_list_str ++ fun ++ ": negative index")

prel_list_str :: String
prel_list_str = "Prelude."
\end{code}
