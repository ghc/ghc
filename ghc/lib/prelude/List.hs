module PreludeList (
	(!!), (++), (\\), all, and, any, break, concat, cycle, drop,
	dropWhile, elem, filter, foldl, foldl1, foldr, foldr1, genericLength,
	head, init, iterate, last, length, lines, map, maximum,
	minimum, notElem, nub, null, or, partition, product, products,
	repeat, reverse, scanl, scanl1, scanr, scanr1, span, splitAt,
	sum, sums, tail, take, takeWhile, transpose, unlines, unwords,
	unzip, unzip3, unzip4, unzip5, unzip6, unzip7, words, zip,
	zip3, zip4, zip5, zip6, zip7, zipWith, zipWith3, zipWith4,
	zipWith5, zipWith6, zipWith7,
	
	_build
    ) where

import Cls
import Core
import IChar		-- instances
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import IList
import ITup2
import Prel		( otherwise, isSpace, (&&), (||), atan2, (.), flip, (^),
			  id, maxInt, maxInt# )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

--infixl 9  !!
--infix  5  \\
--infixr 5  ++
--infix  4 `elem`, `notElem`

-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.

{-# GENERATE_SPECS head a #-}
head			:: [a] -> a
#ifndef USE_FOLDR_BUILD
head (x:_)		=  x
head []			=  error "head{PreludeList}: head []\n"
#else
{-# INLINE head #-}
head 			=  foldr (\ x _ -> x)
				 (error "head{PreludeList}: head []\n") 
#endif

{-# GENERATE_SPECS last a #-}
last			:: [a] -> a
last []			=  error "last{PreludeList}: last []\n"
last [x]		=  x
last (_:xs)		=  last xs

{-# GENERATE_SPECS tail a #-}
tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "tail{PreludeList}: tail []\n"

{-# GENERATE_SPECS init a #-}
init			:: [a] -> [a]
#ifndef USE_FOLDR_BUILD
init []			=  error "init{PreludeList}: init []\n"
init [x]		=  []
init (x:xs)		=  x : init xs
#else
init xs = _build (\ c n ->
  let
 	_init []	=  error "init{PreludeList}: init []\n"
	_init [x]	=  n
	_init (x:xs)	=  x `c` _init xs
  in
        _init xs)
#endif

-- null determines if a list is empty.
{-# GENERATE_SPECS null a #-}
null			:: [a] -> Bool
#ifndef USE_FOLDR_BUILD
null []			= True
null (_:_)		= False
#else
{-# INLINE null #-}
null x			= foldr (\ _ _ -> False) True x
#endif

-- list concatenation (right-associative)
{-# GENERATE_SPECS (++) a #-}
(++)			:: [a] -> [a] -> [a]

#ifdef USE_REPORT_PRELUDE
xs ++ ys		=  foldr (:) ys xs
#else
# ifndef USE_FOLDR_BUILD
[] ++ ys                =  ys
(x:xs) ++ ys            =  x : (xs ++ ys)
# else
{-# INLINE (++) #-}
xs ++ ys		= _augment (\ c n -> foldr c n xs) ys
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  Thus, (xs ++ ys) \\ xs == ys.
{-# GENERATE_SPECS (\\) a{+,Int,[Char]} #-}
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\) xs ys		=  foldl del xs ys
			   where [] `del` _	    = []
				 (x:xs) `del` y
					| x == y    = xs
					| otherwise = x : xs `del` y

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.

{-# GENERATE_SPECS genericLength a{~,Int#,Double#,Int,Integer} b #-}
genericLength		:: (Num a) => [b] -> a
genericLength xs	=  foldl (\n _ -> n+__i1) __i0 xs

{-# GENERATE_SPECS length a #-}
length			:: [a] -> Int
#ifdef USE_REPORT_PRELUDE
length			=  genericLength
#else
# ifndef USE_FOLDR_BUILD
-- stolen from HBC, then unboxified
length l                =  len l 0#
  where
    len :: [a] -> Int# -> Int
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)
# else
{-# INLINE length #-}
length l = foldl (\ n _ -> n+I# 1#) (I# 0#) l
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- List index (subscript) operator, 0-origin

{-# GENERATE_SPECS (!!) a{~,Int#,Int,Integer} b #-}
(!!)			:: (Integral a) => [b] -> a -> b
#ifdef USE_REPORT_PRELUDE
(x:_)  !! 0		=  x
(_:xs) !! (n+1)		=  xs !! n
(_:_)  !! _		=  error "(!!){PreludeList}: negative index"
[]     !! (m+1)		=  error "(!!){PreludeList}: index too large"
#else
-- HBC version (stolen), then unboxified
-- The semantics is not quite the same for error conditions
-- in the more efficient version.
-- (Not to mention if "n" won't fit in an Int :-)

_      !! n | n < __i0  =  error "(!!){PreludeList}: negative index\n"
xs     !! n             =  sub xs (case (toInt n) of { I# n# -> n# })
                           where sub :: [a] -> Int# -> a
                                 sub []      _ = error "(!!){PreludeList}: index too large\n"
                                 sub (x:xs) n# = if n# ==# 0#
						 then x
						 else sub xs (n# `minusInt#` 1#)
#endif /* ! USE_REPORT_PRELUDE */

-- map f xs applies f to each element of xs; i.e., map f xs == [f x | x <- xs].
{-# GENERATE_SPECS map a b #-}
map			:: (a -> b) -> [a] -> [b]
#ifndef USE_FOLDR_BUILD
map f []		=  []
map f (x:xs)		=  f x : map f xs
#else
{-# INLINE map #-}
map f xs		= _build (\ c n -> 
				    foldr (\ a b -> f a `c` b) n xs)
#endif /* USE_FOLDR_BUILD */

-- filter, applied to a predicate and a list, returns the list of those
-- elements that satisfy the predicate; i.e.,
-- filter p xs == [x | x <- xs, p x].
{-# GENERATE_SPECS filter a #-}
filter			:: (a -> Bool) -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
filter p		=  foldr (\x xs -> if p x then x:xs else xs) []
#else
# ifndef USE_FOLDR_BUILD
-- stolen from HBC
filter p []     = []
filter p (x:xs) = if p x then x:filter p xs else filter p xs
# else
{-# INLINE filter #-}
filter f xs = _build (\ c n -> foldr (\ a b -> if f a then c a b else b) n xs)
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */
 
-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i.e.,
-- partition p xs == (filter p xs, filter (not . p) xs).
#ifdef USE_FOLDR_BUILD
{-# INLINE partition #-}
#endif
{-# GENERATE_SPECS partition a #-}
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p xs		=  foldr select ([],[]) xs
			   where select x (ts,fs) | p x	      = (x:ts,fs)
						  | otherwise = (ts,x:fs)

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--	foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--	scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--	scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

{-# GENERATE_SPECS foldl1 a #-}
foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)		=  foldl f x xs
foldl1 _ []		=  error "foldl1{PreludeList}: empty list\n"

{-# GENERATE_SPECS scanl a b#-}
scanl			:: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs		=  q : (case xs of
				[]   -> []
				x:xs -> scanl f (f q x) xs)

{-# GENERATE_SPECS scanl1 a #-}
scanl1			:: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)		=  scanl f x xs
scanl1 _ []		=  error "scanl1{PreludeList}: empty list\n"

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

{-# GENERATE_SPECS foldr1 a #-}
foldr1			:: (a -> a -> a) -> [a] -> a
foldr1 f [x]		=  x
foldr1 f (x:xs)		=  f x (foldr1 f xs)
foldr1 _ []		=  error "foldr1{PreludeList}: empty list\n"

{-# GENERATE_SPECS scanr a b #-}
scanr			:: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []		=  [q0]
scanr f q0 (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr f q0 xs 

{-# GENERATE_SPECS scanr1 a #-}
scanr1			:: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]		=  [x]
scanr1 f  (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr1 f xs 
scanr1 _ []		=  error "scanr1{PreludeList}: empty list\n"

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
{-# GENERATE_SPECS iterate a #-}
iterate			:: (a -> a) -> a -> [a]
#ifndef USE_FOLDR_BUILD
iterate f x		=  x : iterate f (f x)
#else
{-# INLINE iterate #-}
iterate f x		= _build (\ c n -> 
	let
	   _iterate x = x `c` _iterate (f x)
        in 
	   _iterate x)
#endif /* USE_FOLDR_BUILD */


-- repeat x is an infinite list, with x the value of every element.
{-# GENERATE_SPECS repeat a #-}
repeat			:: a -> [a]
#ifndef USE_FOLDR_BUILD
repeat x		=  xs where xs = x:xs
#else
{-# INLINE repeat #-}
repeat x		=  _build (\ c n ->
  	let
 	   xs = x `c` xs
	in
	   xs)
#endif /* USE_FOLDR_BUILD */

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

{-# GENERATE_SPECS cycle a #-}
cycle			:: [a] -> [a]
#ifndef USE_FOLDR_BUILD
cycle xs		=  xs' where xs' = xs ++ xs'
#else
{-# INLINE cycle #-}
cycle xs		=  _build (\ c n ->
				let
				   fx = foldr c fx xs
				in
				   fx)
#endif /* USE_FOLDR_BUILD */

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).

#ifdef USE_REPORT_PRELUDE

take :: (Integral a) => a -> [b] -> [b]
take  0     _		=  []
take  _     []		=  []
take (n+1) (x:xs)	=  x : take n xs

drop :: (Integral a) => a -> [b] -> [b]
drop  0     xs		=  xs
drop  _     []		=  []
drop (n+1) (_:xs)	=  drop n xs

splitAt :: (Integral a) => a -> [b] -> ([b],[b])
splitAt  0     xs	=  ([],xs)
splitAt  _     []	=  ([],[])
splitAt (n+1) (x:xs)	=  (x:xs',xs'') where (xs',xs'') = splitAt n xs

#else /* hack away */


{-# GENERATE_SPECS take a{~,Integer} b #-}
take	:: (Integral a) => a -> [b] -> [b]

{-# SPECIALIZE take :: Int -> [b] -> [b] = _takeInt #-}

#if defined(__UNBOXED_INSTANCES__)
{-# SPECIALIZE take :: 
    Int# -> [b] -> [b] 			= _takeUInt,
    Int# -> [Int#] -> [Int#] 		= _takeUInti,
    Int# -> [Char#] -> [Char#] 		= _takeUIntc,
    Int# -> [Double#] -> [Double#] 	= _takeUIntd,
    Int -> [Int#] -> [Int#] 		= _takeInti,
    Int -> [Char#] -> [Char#] 		= _takeIntc,
    Int -> [Double#] -> [Double#] 	= _takeIntd #-}

{-# INLINE _takeUInti #-}
{-# INLINE _takeUIntc #-}
{-# INLINE _takeUIntd #-}
{-# INLINE _takeInti  #-}
{-# INLINE _takeIntc  #-}
{-# INLINE _takeIntd  #-}

_takeUInti :: Int# -> [Int#] -> [Int#]
_takeUInti n xs = _takeUInt n xs
_takeUIntc :: Int# -> [Char#] -> [Char#] 
_takeUIntc n xs = _takeUInt n xs
_takeUIntd :: Int# -> [Double#] -> [Double#]
_takeUIntd n xs = _takeUInt n xs
_takeInti :: Int -> [Int#] -> [Int#]
_takeInti n xs = _takeInt n xs
_takeIntc :: Int -> [Char#] -> [Char#] 
_takeIntc n xs = _takeInt n xs
_takeIntd :: Int -> [Double#] -> [Double#]
_takeIntd n xs = _takeInt n xs

#endif

-- The general code for take, below, checks n <= maxInt
-- No need to check for maxInt overflow when specialised
-- at type Int or Int# since the Int must be <= maxInt

_takeUInt :: Int# -> [b] -> [b]
_takeUInt n xs
  | n `geInt#` 0# =  _take_unsafe_UInt n xs
  | otherwise 	  =  error "take{PreludeList}: negative index"

{-# INLINE _takeInt   #-}
_takeInt :: Int -> [b] -> [b]
_takeInt (I# n#) xs = _takeUInt n# xs 

_take_unsafe_UInt 0# _     = []
_take_unsafe_UInt _  []	   = []
_take_unsafe_UInt m (x:xs) = x : _take_unsafe_UInt (m `minusInt#` 1#) xs

-- For an index n between maxInt and maxInt^2 we use a function
-- with two indexes m and r where n = m * maxInt + r

_take_unsafe_UIntUInt _  _   []    = []
_take_unsafe_UIntUInt 1# 0#  xs    = _take_unsafe_UInt maxInt# xs
_take_unsafe_UIntUInt m  0#  xs    = _take_unsafe_UIntUInt (m `minusInt#` 1#) maxInt# xs
_take_unsafe_UIntUInt m  r  (x:xs) = x : _take_unsafe_UIntUInt m (r `minusInt#` 1#) xs

_take_unsafe_Integral :: (Integral a) => a -> [b] -> [b]
_take_unsafe_Integral _  []    =  []
_take_unsafe_Integral 0  _     =  []
_take_unsafe_Integral n (x:xs) =  x : _take_unsafe_Integral (n-1) xs

__max :: Num a => a
__max = fromInt maxInt

#ifndef USE_FOLDR_BUILD
take n | n < __i0
       = error "take{PreludeList}: negative index"
       | n <= __max
       = let n# = i2i# (toInt n)
	 in \xs -> _take_unsafe_UInt n# xs
       | n <= __max * __max
       = let m# = i2i# (toInt m)
	     r# = i2i# (toInt r)
	 in \xs -> _take_unsafe_UIntUInt m# r# xs
       | otherwise
       = \xs -> _take_unsafe_Integral n xs
       where
	 (m,r) = n `quotRem` __max
	 i2i# (I# i#) = i#
#else
{-# INLINE take #-}
take n xs = takeInt (toInt n) xs

{-# INLINE takeInt #-}
takeInt			:: Int -> [b] -> [b]
takeInt n xs = _build (\ c0 n0 ->
 let
	takeInt# 0# _       = n0
	takeInt# _  []	    = n0
	takeInt# m# (x:xs)  = x `c0` takeInt# (m# `minusInt#` 1#) xs
  in
    case n of
      I# n# -> if n# <# 0# 
	       then error "take{PreludeList}: negative index"
	       else takeInt# n# xs)

#endif /* USE_FOLDR_BUILD */

-- Test
-- main = print (head (take (123456789123456789::Integer) [1..]))

-- ToDo: NEW drop and splitAt, too (WDP)

{-# GENERATE_SPECS drop a{~,Int#,Int,Integer} b #-}
drop	:: (Integral a) => a -> [b] -> [b]
drop n xs = dropInt (toInt n) xs

dropInt			:: Int -> [b] -> [b]
dropInt (I# n#) xs
  | n# <# 0#	= error "drop{PreludeList}: negative index"
  | otherwise	= dropInt# n# xs
    where
	dropInt# :: Int# -> [a] -> [a]
	dropInt# 0# xs      = xs
	dropInt# _  []	    = []
	dropInt# m# (_:xs)  = dropInt# (m# `minusInt#` 1#) xs

{-# GENERATE_SPECS splitAt a{~,Int#,Int,Integer} b #-}
splitAt	:: (Integral a) => a -> [b] -> ([b], [b])
splitAt  n  xs | n >= 0 = splitAtInt (toInt n) xs

splitAtInt		:: Int -> [b] -> ([b], [b])
splitAtInt (I# n#) xs
  | n# <# 0#	= error "splitAt{PreludeList}: negative index"
  | otherwise	= splitAtInt# n# xs
    where
	splitAtInt# :: Int# -> [a] -> ([a], [a])
	splitAtInt# 0# xs	= ([], xs)
	splitAtInt# _  []	= ([], [])
	splitAtInt# m# (x:xs)	= (x:xs', xs'')
	  where
	    (xs', xs'') = splitAtInt# (m# `minusInt#` 1#) xs

#endif /* USE_REPORT_PRELUDE */

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

{-# GENERATE_SPECS takeWhile a #-}
takeWhile		:: (a -> Bool) -> [a] -> [a]
#ifndef USE_FOLDR_BUILD
takeWhile p []		=  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []
#else
{-# INLINE takeWhile #-}
takeWhile p xs		= _build (\ c n -> 
    let
	fn x r = if  p x
		 then x `c` r
	         else n
    in
	foldr fn n xs)
#endif /* USE_FOLDR_BUILD */

{-# GENERATE_SPECS dropWhile a #-}
dropWhile		:: (a -> Bool) -> [a] -> [a]
dropWhile p []		=  []
dropWhile p xs@(x:xs')
	    | p x       =  dropWhile p xs'
	    | otherwise =  xs

{-# GENERATE_SPECS span a #-}
span			:: (a -> Bool) -> [a] -> ([a],[a])
span p []		=  ([],[])
span p xs@(x:xs')
	   | p x	=  let (ys,zs) = span p xs' in (x:ys,zs)
	   | otherwise	=  ([],xs)

{-# GENERATE_SPECS break a #-}
break		:: (a -> Bool) -> [a] -> ([a],[a])
#ifdef USE_REPORT_PRELUDE
break p			= span (not . p)
#else
-- HBC version (stolen)
break p []		=  ([],[])
break p xs@(x:xs')
	   | p x	=  ([],xs)
	   | otherwise	=  let (ys,zs) = break p xs' in (x:ys,zs)
#endif

-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.

lines			:: String -> [String]
lines ""		=  []
lines s			=  let (l, s') = break (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> lines s''

words			:: String -> [String]
#ifndef USE_FOLDR_BUILD
words s			=  case dropWhile isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = break isSpace s'
#else
{-# INLINE words #-}
words s = _build (\ c n ->
  let
	words' s =  case dropWhile isSpace s of
				"" -> n
				s' -> w `c` words' s''
				      where (w, s'') = break isSpace s'
  in
	words' s)
#endif /* USE_FOLDR_BUILD */

unlines			:: [String] -> String
#ifdef USE_REPORT_PRELUDE
unlines			=  concat . map (++ "\n")
#else
# ifndef USE_FOLDR_BUILD
-- HBC version (stolen)
-- here's a more efficient version
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls

# else
{-# INLINE unlines #-}
unlines xs = _build (\ c n -> foldr (\ l r -> foldr c ('\n' `c` r) l) n xs)

# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

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
#endif /* ! USE_REPORT_PRELUDE */

-- nub (meaning "essence") removes duplicate elements from its list argument.
{-# GENERATE_SPECS nub a{+,Int,[Int],[Char]} #-}
nub			:: (Eq a) => [a] -> [a]
#ifdef USE_REPORT_PRELUDE
nub []			=  []
nub (x:xs)		=  x : nub (filter (/= x) xs)
#else
# ifndef USE_FOLDR_BUILD
-- stolen from HBC
nub l                   = nub' l []
  where
    nub' [] _		= []
    nub' (x:xs) l	= if x `elem` l then nub' xs l else x : nub' xs (x:l)
# else
{-# INLINE nub #-}
nub l                   = _build (\ c n -> 
 let
    nub' [] _		= n
    nub' (x:xs) l	= if x `elem` l then nub' xs l else x `c` nub' xs (x:l)
 in
    nub' l [])
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
{-# GENERATE_SPECS reverse a #-}
reverse			:: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
reverse			=  foldl (flip (:)) []
#else
# ifndef USE_FOLDR_BUILD
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
# else
{-# INLINE reverse #-}
reverse	xs = _build (\ c n -> foldl (\ a b -> c b a) n xs)
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or			:: [Bool] -> Bool
#ifdef USE_REPORT_PRELUDE
and			=  foldr (&&) True
or			=  foldr (||) False
#else
# ifndef USE_FOLDR_BUILD
and []		=  True
and (x:xs)	=  x && and xs
or []		=  False
or (x:xs)	=  x || or xs
# else
{-# INLINE and #-}
{-# INLINE or #-}
and			=  foldr (&&) True
or			=  foldr (||) False
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
{-# GENERATE_SPECS any a #-}
any			:: (a -> Bool) -> [a] -> Bool
{-# GENERATE_SPECS all a #-}
all			:: (a -> Bool) -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
any p			= or . map p
all p			= and . map p
#else
# ifndef USE_FOLDR_BUILD
any p []	= False
any p (x:xs)	= p x || any p xs
all p []	=  True
all p (x:xs)	=  p x && all p xs
# else
{-# INLINE any #-}
{-# INLINE all #-}
-- We expand these out, so as the non-deforested versions
-- that use the f/b prelude can get a fair run for comparisons.
any p xs		=  foldr (\ x r -> p x || r) False xs
all p xs		=  foldr (\ x r -> p x && r) True xs
# endif
#endif /* ! USE_REPORT_PRELUDE */

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
{-# GENERATE_SPECS elem a{+,Int,Integer,Char,String,(Int,Int)} #-}
elem			:: (Eq a) => a -> [a] -> Bool
{-# GENERATE_SPECS notElem a{+,Int,Integer,Char,String,(Int,Int)} #-}
notElem			:: (Eq a) => a -> [a] -> Bool

#ifdef USE_REPORT_PRELUDE
elem			=  any . (==)
notElem			=  all . (/=)
#else

# ifndef USE_FOLDR_BUILD
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys

notElem	x []	=  True
notElem x (y:ys)=  x /= y && notElem x ys

# else
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys

notElem	x []	=  True
notElem x (y:ys)=  x /= y && notElem x ys

-- Put back later ....
--{-# INLINE elem #-}
--{-# INLINE notElem #-}
----- We are prepared to lose the partial application to equality,
---- ie (x ==), and replace it with (\ y -> x == y)
--elem x ys 		=  any (\ y -> x == y) ys
--notElem	x ys		=  all (\ y -> x /= y) ys
# endif /* USE_FOLDR_BUILD */
#endif /* ! USE_REPORT_PRELUDE */

-- sum and product compute the sum or product of a finite list of numbers.
{-# GENERATE_SPECS sum a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double)} #-}
sum			:: (Num a) => [a] -> a
{-# GENERATE_SPECS product a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double)} #-}
product			:: (Num a) => [a] -> a

#ifdef USE_REPORT_PRELUDE
sum			=  foldl (+) 0	
product			=  foldl (*) 1
#else

# ifndef USE_FOLDR_BUILD
sum	l	= sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
product	l	= prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
# else
{-# INLINE sum #-}
{-# INLINE product #-}
sum xs			=  foldl (+) lit0 xs 
  where lit0 = 0
product xs		=  foldl (*) lit1 xs
  where lit1 = 1
# endif
#endif /* ! USE_REPORT_PRELUDE */

-- sums and products give a list of running sums or products from
-- a list of numbers.  For example,  sums [1,2,3] == [0,1,3,6].
{-# GENERATE_SPECS sums a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double)} #-}
sums			:: (Num a) => [a] -> [a]
sums xs			=  scanl (+) __i0 xs
{-# GENERATE_SPECS products a{Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double)} #-}
products		:: (Num a) => [a] -> [a]
products xs		=  scanl (*) __i1 xs

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
{-# GENERATE_SPECS maximum a{+,Int,Integer,Double} #-}
maximum			:: (Ord a) => [a] -> a
{-# GENERATE_SPECS minimum a{+,Int,Integer,Double} #-}
minimum			:: (Ord a) => [a] -> a
#ifdef USE_REPORT_PRELUDE
maximum			=  foldl1 max
minimum			=  foldl1 min
#else
maximum	[x]	= x
maximum (x:xs)	= max x (maximum xs)
minimum	[x]	= x
minimum (x:xs)	= min x (minimum xs)
#endif /* ! USE_REPORT_PRELUDE */

-- concat, applied to a list of lists, returns their flattened concatenation.
{-# GENERATE_SPECS concat a #-}
concat			:: [[a]] -> [a]
#ifdef USE_REPORT_PRELUDE
concat			=  foldr (++) []
#else
# ifndef USE_FOLDR_BUILD
-- HBC version (stolen)
concat []		= []
concat ([]:xss)		= concat xss                    -- for better stack behaviour!
--NO:bad strictness: concat ([x]:xss)	= x : concat xss -- this should help too ???
concat ((y:ys):xss)     = y : (ys ++ concat xss)
# else
{-# INLINE concat #-}
concat xs = _build (\ c n -> foldr (\ x y -> foldr c y x) n xs)
# endif
#endif /* ! USE_REPORT_PRELUDE */

-- transpose, applied to a list of lists, returns that list with the
-- "rows" and "columns" interchanged.  The input need not be rectangular
-- (a list of equal-length lists) to be completely transposable, but can
-- be "triangular":  Each successive component list must be not longer
-- than the previous one; any elements outside of the "triangular"
-- transposable region are lost.  The input can be infinite in either
-- dimension or both.
{-# GENERATE_SPECS transpose a #-}
transpose		:: [[a]] -> [[a]]
transpose xs		=  foldr 
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     [] xs

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.
-- zip3 takes three lists and returns a list of triples, etc.  Versions
-- of zip producing up to septuplets are defined here.

#ifdef USE_FOLDR_BUILD
{-# INLINE zip #-}
#endif
{-# GENERATE_SPECS zip a b #-}
zip			:: [a] -> [b] -> [(a,b)]
zip as bs		=  zipWith (\a b -> (a,b)) as bs

zip3			:: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 as bs cs		=  zipWith3 (\a b c -> (a,b,c)) as bs cs

zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 as bs cs ds	=  zipWith4 (\a b c d -> (a,b,c,d)) as bs cs ds

zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 as bs cs ds es	=  zipWith5 (\a b c d e -> (a,b,c,d,e)) as bs cs ds es

zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f]
			   -> [(a,b,c,d,e,f)]
zip6 as bs cs ds es fs	=  zipWith6 (\a b c d e f -> (a,b,c,d,e,f)) as bs cs ds es fs

zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
			   -> [(a,b,c,d,e,f,g)]
zip7 as bs cs ds es fs gs =  zipWith7 (\a b c d e f g -> (a,b,c,d,e,f,g)) as bs cs ds es fs gs

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.

{-# GENERATE_SPECS zipWith a b c #-}
zipWith			:: (a->b->c) -> [a]->[b]->[c]
#ifndef USE_FOLDR_BUILD
zipWith z (a:as) (b:bs)	=  z a b : zipWith z as bs
zipWith _ _ _		=  []
#else
{-# INLINE zipWith #-}
zipWith z xs ys = _build (\ c n ->
		let
		   h (a:as) (b:bs) = z a b `c` h as bs
		   h _      _      = n
		in
		   h xs ys)
#endif



zipWith3		:: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
			=  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _	=  []

zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
			=  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _	=  []

zipWith5		:: (a->b->c->d->e->f)
			   -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
			=  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _	=  []

zipWith6		:: (a->b->c->d->e->f->g)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
			=  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _	=  []

zipWith7		:: (a->b->c->d->e->f->g->h)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
		   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ =  []


-- unzip transforms a list of pairs into a pair of lists.  As with zip,
-- a family of such functions up to septuplets is provided.

#ifdef USE_FOLDR_BUILD
{-# INLINE unzip #-}
#endif

{-# GENERATE_SPECS unzip a b #-}
unzip			:: [(a,b)] -> ([a],[b])
unzip xs		=  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[]) xs

unzip3			:: [(a,b,c)] -> ([a],[b],[c])
unzip3 xs		=  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				 ([],[],[]) xs

unzip4			:: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 xs		=  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
					(a:as,b:bs,c:cs,d:ds))
				 ([],[],[],[]) xs

unzip5			:: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 xs		=  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
					(a:as,b:bs,c:cs,d:ds,e:es))
				 ([],[],[],[],[]) xs

unzip6			:: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6 xs		=  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs))
				 ([],[],[],[],[],[]) xs

unzip7			:: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7 xs		=  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
				 ([],[],[],[],[],[],[]) xs
