%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[List]{Module @Lhar@}

\begin{code}
module List ( 
    {- 
      This list follows the type signatures for the
      standard List interface.  -- 8/97 
    -}
    elemIndex, elemIndices,
    find, findIndex, findIndices,
    nub, nubBy, 
    delete, deleteBy, (\\), deleteFirstsBy,
    union, unionBy, 
    intersect, intersectBy,
    intersperse, transpose, partition, 
    group, groupBy,
    inits, tails,
    isPrefixOf, isSuffixOf,
    mapAccumL, mapAccumR,
    sort, sortBy, 
    insertBy, 
    maximumBy, minimumBy,
    genericTake,  genericDrop, genericSplitAt, 
    genericIndex, genericReplicate, genericLength, 
    
    zip4, zip5, zip6, zip7,
    zipWith4, zipWith5, zipWith6, zipWith7,
    unzip4, unzip5, unzip6, unzip7

  ) where

import Prelude
import Maybe	(listToMaybe)
import PrelBase	( Int(..) )
import GHC	( (+#) )

infix 5 \\
\end{code}

%*********************************************************
%*							*
\subsection{List functions}
%*							*
%*********************************************************

\begin{code}
elemIndex	:: Eq a => a -> [a] -> Maybe Int
elemIndex x     = findIndex (x==)

elemIndices     :: Eq a => a -> [a] -> [Int]
elemIndices x   = findIndices (x==)

find		:: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

findIndex       :: (a -> Bool) -> [a] -> Maybe Int
findIndex p     = listToMaybe . findIndices p

findIndices      :: (a -> Bool) -> [a] -> [Int]

-- One line definition
-- findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]

-- Efficient definition
findIndices p xs = loop 0# p xs
		 where
	 	   loop n p [] = []
		   loop n p (x:xs) | p x       = I# n : loop (n +# 1#) p xs
				   | otherwise = loop (n +# 1#) p xs

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
nub l                   = nub' l []
  where
    nub' [] _		= []
    nub' (x:xs) l	= if x `elem` l then nub' xs l else x : nub' xs (x:l)
#endif

nubBy			:: (a -> a -> Bool) -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
#else
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _		= []
    nubBy' (x:xs) l	= if elemBy eq x l then nubBy' xs l else x : nubBy' xs (x:l)

--not exported:
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []		=  False
elemBy eq x (y:ys)	=  x `eq` y || elemBy eq x ys
#endif


-- delete x removes the first occurrence of x from its list argument.
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
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
intersperse sep []      = []
intersperse sep [x]     = [x]
intersperse sep (x:xs)  = x : sep : intersperse sep xs

transpose		:: [[a]] -> [[a]]
transpose		=  foldr
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     []


-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i,e,,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p xs		=  foldr select ([],[]) xs
			   where select x (ts,fs) | p x       = (x:ts,fs)
                                                  | otherwise = (ts, x:fs)
\end{code}

@mapAccumL@ behaves like a combination
of  @map@ and @foldl@;
it applies a function to each element of a list, passing an accumulating
parameter from left to right, and returning a final value of this
accumulator together with the new list.

\begin{code}

mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
				    -- and accumulator, returning new
				    -- accumulator and elt of result list
   	  -> acc	    -- Initial accumulator 
	  -> [x]	    -- Input list
	  -> (acc, [y])	    -- Final accumulator and result list
mapAccumL f s []     	=  (s, [])
mapAccumL f s (x:xs) 	=  (s'',y:ys)
		           where (s', y ) = f s x
			         (s'',ys) = mapAccumL f s' xs
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
mapAccumR f s []     	=  (s, [])
mapAccumR f s (x:xs)	=  (s'', y:ys)
		           where (s'',y ) = f s' x
			         (s', ys) = mapAccumR f s xs
\end{code}

\begin{code}
sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr (insertBy cmp) []

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy cmp x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys

maximumBy		:: (a -> a -> a) -> [a] -> a
maximumBy max []	=  error "List.maximumBy: empty list"
maximumBy max xs	=  foldl1 max xs

minimumBy		:: (a -> a -> a) -> [a] -> a
minimumBy min []	=  error "List.minimumBy: empty list"
minimumBy min xs	=  foldl1 min xs

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
groupBy eq []		=  []
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

\end{code}
