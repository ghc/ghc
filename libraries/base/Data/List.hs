{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on lists.
--
-----------------------------------------------------------------------------

module Data.List
   ( 
    [] (..),

   , elemIndex	       -- :: (Eq a) => a -> [a] -> Maybe Int
   , elemIndices       -- :: (Eq a) => a -> [a] -> [Int]

   , find	       -- :: (a -> Bool) -> [a] -> Maybe a
   , findIndex	       -- :: (a -> Bool) -> [a] -> Maybe Int
   , findIndices       -- :: (a -> Bool) -> [a] -> [Int]
   
   , nub               -- :: (Eq a) => [a] -> [a]
   , nubBy             -- :: (a -> a -> Bool) -> [a] -> [a]

   , delete            -- :: (Eq a) => a -> [a] -> [a]
   , deleteBy          -- :: (a -> a -> Bool) -> a -> [a] -> [a]
   , (\\)              -- :: (Eq a) => [a] -> [a] -> [a]
   , deleteFirstsBy    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   
   , union             -- :: (Eq a) => [a] -> [a] -> [a]
   , unionBy           -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]

   , intersect         -- :: (Eq a) => [a] -> [a] -> [a]
   , intersectBy       -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]

   , intersperse       -- :: a -> [a] -> [a]
   , transpose         -- :: [[a]] -> [[a]]
   , partition         -- :: (a -> Bool) -> [a] -> ([a], [a])

   , group             -- :: Eq a => [a] -> [[a]]
   , groupBy           -- :: (a -> a -> Bool) -> [a] -> [[a]]

   , inits             -- :: [a] -> [[a]]
   , tails             -- :: [a] -> [[a]]

   , isPrefixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   , isSuffixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   
   , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
   , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
   
   , sort              -- :: (Ord a) => [a] -> [a]
   , sortBy            -- :: (a -> a -> Ordering) -> [a] -> [a]
   
   , insert            -- :: (Ord a) => a -> [a] -> [a]
   , insertBy          -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
   
   , maximumBy	       -- :: (a -> a -> Ordering) -> [a] -> a
   , minimumBy         -- :: (a -> a -> Ordering) -> [a] -> a
   
   , genericLength     -- :: (Integral a) => [b] -> a
   , genericTake       -- :: (Integral a) => a -> [b] -> [b]
   , genericDrop       -- :: (Integral a) => a -> [b] -> [b]
   , genericSplitAt    -- :: (Integral a) => a -> [b] -> ([b], [b])
   , genericIndex      -- :: (Integral a) => [b] -> a -> b
   , genericReplicate  -- :: (Integral a) => a -> b -> [b]
   
   , unfoldr		-- :: (b -> Maybe (a, b)) -> b -> [a]

   , zip4, zip5, zip6, zip7
   , zipWith4, zipWith5, zipWith6, zipWith7
   , unzip4, unzip5, unzip6, unzip7

   , map               -- :: ( a -> b ) -> [a] -> [b]
   , (++)	       -- :: [a] -> [a] -> [a]
   , concat            -- :: [[a]] -> [a]
   , filter	       -- :: (a -> Bool) -> [a] -> [a]
   , head	       -- :: [a] -> a
   , last	       -- :: [a] -> a
   , tail	       -- :: [a] -> [a]
   , init              -- :: [a] -> [a]
   , null	       -- :: [a] -> Bool
   , length	       -- :: [a] -> Int
   , (!!)	       -- :: [a] -> Int -> a
   , foldl	       -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl'	       -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl1	       -- :: (a -> a -> a) -> [a] -> a
   , scanl             -- :: (a -> b -> a) -> a -> [b] -> [a]
   , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
   , foldr             -- :: (a -> b -> b) -> b -> [a] -> b
   , foldr1            -- :: (a -> a -> a) -> [a] -> a
   , scanr             -- :: (a -> b -> b) -> b -> [a] -> [b]
   , scanr1            -- :: (a -> a -> a) -> [a] -> [a]
   , iterate           -- :: (a -> a) -> a -> [a]
   , repeat            -- :: a -> [a]
   , replicate         -- :: Int -> a -> [a]
   , cycle             -- :: [a] -> [a]
   , take              -- :: Int -> [a] -> [a]
   , drop              -- :: Int -> [a] -> [a]
   , splitAt           -- :: Int -> [a] -> ([a], [a])
   , takeWhile         -- :: (a -> Bool) -> [a] -> [a]
   , dropWhile         -- :: (a -> Bool) -> [a] -> [a]
   , span              -- :: (a -> Bool) -> [a] -> ([a], [a])
   , break             -- :: (a -> Bool) -> [a] -> ([a], [a])

   , lines	       -- :: String   -> [String]
   , words	       -- :: String   -> [String]
   , unlines           -- :: [String] -> String
   , unwords           -- :: [String] -> String
   , reverse           -- :: [a] -> [a]
   , and	       -- :: [Bool] -> Bool
   , or                -- :: [Bool] -> Bool
   , any               -- :: (a -> Bool) -> [a] -> Bool
   , all               -- :: (a -> Bool) -> [a] -> Bool
   , elem              -- :: a -> [a] -> Bool
   , notElem           -- :: a -> [a] -> Bool
   , lookup            -- :: (Eq a) => a -> [(a,b)] -> Maybe b
   , sum               -- :: (Num a) => [a] -> a
   , product           -- :: (Num a) => [a] -> a
   , maximum           -- :: (Ord a) => [a] -> a
   , minimum           -- :: (Ord a) => [a] -> a
   , concatMap         -- :: (a -> [b]) -> [a] -> [b]
   , zip               -- :: [a] -> [b] -> [(a,b)]
   , zip3  
   , zipWith           -- :: (a -> b -> c) -> [a] -> [b] -> [c]
   , zipWith3
   , unzip             -- :: [(a,b)] -> ([a],[b])
   , unzip3

   ) where

import Data.Maybe

#ifdef __GLASGOW_HASKELL__
import GHC.Num
import GHC.Real
import GHC.List
import GHC.Show	( lines, words, unlines, unwords )
import GHC.Base
#endif

infix 5 \\ 

-- -----------------------------------------------------------------------------
-- List functions

elemIndex	:: Eq a => a -> [a] -> Maybe Int
elemIndex x     = findIndex (x==)

elemIndices     :: Eq a => a -> [a] -> [Int]
elemIndices x   = findIndices (x==)

find		:: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

findIndex       :: (a -> Bool) -> [a] -> Maybe Int
findIndex p     = listToMaybe . findIndices p

findIndices      :: (a -> Bool) -> [a] -> [Int]

#ifdef USE_REPORT_PRELUDE
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
#else
#ifdef __HUGS__
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
#else 
-- Efficient definition
findIndices p ls = loop 0# ls
		 where
	 	   loop _ [] = []
		   loop n (x:xs) | p x       = I# n : loop (n +# 1#) xs
				 | otherwise = loop (n +# 1#) xs
#endif  /* __HUGS__ */
#endif  /* USE_REPORT_PRELUDE */

isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf x y          =  reverse x `isPrefixOf` reverse y

-- nub (meaning "essence") remove duplicate elements from its list argument.
nub                     :: (Eq a) => [a] -> [a]
#ifdef USE_REPORT_PRELUDE
nub                     =  nubBy (==)
#else
-- stolen from HBC
nub l                   = nub' l []		-- '
  where
    nub' [] _		= []			-- '
    nub' (x:xs) ls				-- '
	| x `elem` ls   = nub' xs ls		-- '
	| otherwise     = x : nub' xs (x:ls)	-- '
#endif

nubBy			:: (a -> a -> Bool) -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
#else
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _		= []
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs 
       | otherwise	 = y : nubBy' ys (y:xs)

-- Not exported:
-- Note that we keep the call to `eq` with arguments in the
-- same order as in the reference implementation
-- 'xs' is the list of things we've seen so far, 
-- 'y' is the potential new element
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []		=  False
elem_by eq y (x:xs)	=  x `eq` y || elem_by eq y xs
#endif


-- delete x removes the first occurrence of x from its list argument.
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  Thus, (xs ++ ys) \\ xs == ys.
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\)		        =  foldl (flip delete)

-- List union, remove the elements of first list from second.
union			:: (Eq a) => [a] -> [a] -> [a]
union 			= unionBy (==)

unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect               =  intersectBy (==)

intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]

-- intersperse sep inserts sep between the elements of its list argument.
-- e.g. intersperse ',' "abcde" == "a,b,c,d,e"
intersperse		:: a -> [a] -> [a]
intersperse _   []      = []
intersperse _   [x]     = [x]
intersperse sep (x:xs)  = x : sep : intersperse sep xs

transpose		:: [[a]] -> [[a]]
transpose []		 = []
transpose ([]	: xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:t) <- xss]) : transpose (xs : [ t | (h:t) <- xss])


-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i,e,,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition		:: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs

select p x (ts,fs) | p x       = (x:ts,fs)
                   | otherwise = (ts, x:fs)

-- @mapAccumL@ behaves like a combination
-- of  @map@ and @foldl@;
-- it applies a function to each element of a list, passing an accumulating
-- parameter from left to right, and returning a final value of this
-- accumulator together with the new list.

mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
				    -- and accumulator, returning new
				    -- accumulator and elt of result list
   	  -> acc	    -- Initial accumulator 
	  -> [x]	    -- Input list
	  -> (acc, [y])	    -- Final accumulator and result list
mapAccumL _ s []     	=  (s, [])
mapAccumL f s (x:xs) 	=  (s'',y:ys)
		           where (s', y ) = f s x
			         (s'',ys) = mapAccumL f s' xs

-- @mapAccumR@ does the same, but working from right to left instead.
-- Its type is the same as @mapAccumL@, though.

mapAccumR :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
					-- and accumulator, returning new
					-- accumulator and elt of result list
	    -> acc 		-- Initial accumulator
	    -> [x] 		-- Input list
	    -> (acc, [y])		-- Final accumulator and result list
mapAccumR _ s []     	=  (s, [])
mapAccumR f s (x:xs)	=  (s'', y:ys)
		           where (s'',y ) = f s' x
			         (s', ys) = mapAccumR f s xs


insert :: Ord a => a -> [a] -> [a]
insert e ls = insertBy (compare) e ls

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys

maximumBy		:: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []		=  error "List.maximumBy: empty list"
maximumBy cmp xs	=  foldl1 max xs
			where
			   max x y = case cmp x y of
					GT -> x
					_  -> y

minimumBy		:: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []		=  error "List.minimumBy: empty list"
minimumBy cmp xs	=  foldl1 min xs
			where
			   min x y = case cmp x y of
					GT -> y
					_  -> x

genericLength           :: (Num i) => [b] -> i
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l

genericTake		:: (Integral i) => i -> [a] -> [a]
genericTake 0 _         =  []
genericTake _ []        =  []
genericTake n (x:xs) | n > 0  =  x : genericTake (n-1) xs
genericTake _  _        =  error "List.genericTake: negative argument"

genericDrop		:: (Integral i) => i -> [a] -> [a]
genericDrop 0 xs        =  xs
genericDrop _ []        =  []
genericDrop n (_:xs) | n > 0  =  genericDrop (n-1) xs
genericDrop _ _		=  error "List.genericDrop: negative argument"

genericSplitAt          :: (Integral i) => i -> [b] -> ([b],[b])
genericSplitAt 0 xs     =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) | n > 0  =  (x:xs',xs'') where
                               (xs',xs'') = genericSplitAt (n-1) xs
genericSplitAt _ _      =  error "List.genericSplitAt: negative argument"


genericIndex :: (Integral a) => [b] -> a -> b
genericIndex (x:_)  0 = x
genericIndex (_:xs) n 
 | n > 0     = genericIndex xs (n-1)
 | otherwise = error "List.genericIndex: negative argument."
genericIndex _ _      = error "List.genericIndex: index too large."

genericReplicate	:: (Integral i) => i -> a -> [a]
genericReplicate n x	=  genericTake n (repeat x)


zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4			=  zipWith4 (,,,)

zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5			=  zipWith5 (,,,,)

zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> 
                              [(a,b,c,d,e,f)]
zip6			=  zipWith6 (,,,,,)

zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
zip7			=  zipWith7 (,,,,,,)

zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
			=  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _	=  []

zipWith5		:: (a->b->c->d->e->f) -> 
                           [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
			=  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _	= []

zipWith6		:: (a->b->c->d->e->f->g) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
			=  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _	= []

zipWith7		:: (a->b->c->d->e->f->g->h) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
		   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

unzip4			:: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4			=  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
					(a:as,b:bs,c:cs,d:ds))
				 ([],[],[],[])

unzip5			:: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5			=  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
					(a:as,b:bs,c:cs,d:ds,e:es))
				 ([],[],[],[],[])

unzip6			:: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6			=  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs))
				 ([],[],[],[],[],[])

unzip7		:: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7		=  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
				(a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
			 ([],[],[],[],[],[],[])



deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))


-- group splits its list argument into a list of lists of equal, adjacent
-- elements.  e.g.,
-- group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
group                   :: (Eq a) => [a] -> [[a]]
group                   =  groupBy (==)

groupBy 		:: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []		=  []
groupBy eq (x:xs)	=  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- inits xs returns the list of initial segments of xs, shortest first.
-- e.g., inits "abc" == ["","a","ab","abc"]
inits 			:: [a] -> [[a]]
inits []		=  [[]]
inits (x:xs) 		=  [[]] ++ map (x:) (inits xs)

-- tails xs returns the list of all final segments of xs, longest first.
-- e.g., tails "abc" == ["abc", "bc", "c",""]
tails 			:: [a] -> [[a]]
tails []	        =  [[]]
tails xxs@(_:xs) 	=  xxs : tails xs


------------------------------------------------------------------------------
-- Quick Sort algorithm taken from HBC's QSort library.

sort :: (Ord a) => [a] -> [a]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]

#ifdef USE_REPORT_PRELUDE
sort = sortBy compare
sortBy cmp = foldr (insertBy cmp) []
#else

sortBy cmp l = qsort cmp l []
sort l = qsort compare l []

-- rest is not exported:

-- qsort is stable and does not concatenate.
qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
qsort _   []     r = r
qsort _   [x]    r = x:r
qsort cmp (x:xs) r = qpart cmp x xs [] [] r

-- qpart partitions and sorts the sublists
qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart cmp x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort cmp rlt (x:rqsort cmp rge r)
qpart cmp x (y:ys) rlt rge r =
    case cmp x y of
	GT -> qpart cmp x ys (y:rlt) rge r
        _  -> qpart cmp x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
rqsort _   []     r = r
rqsort _   [x]    r = x:r
rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r

rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart cmp x [] rle rgt r =
    qsort cmp rle (x:qsort cmp rgt r)
rqpart cmp x (y:ys) rle rgt r =
    case cmp y x of
	GT -> rqpart cmp x ys rle (y:rgt) r
    	_  -> rqpart cmp x ys (y:rle) rgt r

#endif /* USE_REPORT_PRELUDE */

{-
\begin{verbatim}
  unfoldr f' (foldr f z xs) == (z,xs)

 if the following holds:

   f' (f x y) = Just (x,y)
   f' z       = Nothing
\end{verbatim}
-}

unfoldr      :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b  =
  case f b of
   Just (a,new_b) -> a : unfoldr f new_b
   Nothing        -> []


-- -----------------------------------------------------------------------------
-- strict version of foldl

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = let a' = f a x in a' `seq` foldl' f a' xs

-- -----------------------------------------------------------------------------
-- List sum and product

-- sum and product compute the sum or product of a finite list of numbers.
{-# SPECIALISE sum     :: [Int] -> Int #-}
{-# SPECIALISE sum     :: [Integer] -> Integer #-}
{-# SPECIALISE product :: [Int] -> Int #-}
{-# SPECIALISE product :: [Integer] -> Integer #-}
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
