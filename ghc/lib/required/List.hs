module List ( 
    delete, deleteBy, (\\), deleteFirsts, deleteFirstsBy,
    elemBy, notElemBy, lookupBy, maximumBy, minimumBy,
    nub, nubBy, partition, sums, products, transpose,
    zip4, zip5, zip6, zip7,
    zipWith4, zipWith5, zipWith6, zipWith7,
    unzip4, unzip5, unzip6, unzip7,
    genericLength, genericDrop, genericTake, genericSplitAt,
    genericReplicate,
    elemIndex, elemIndexBy, intersperse, group, groupBy,
    mapAccumL, mapAccumR,
    inits, tails, subsequences, permutations, 
    union, intersect ) where

-- delete x removes the first occurrence of x from its list argument.
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else deleteBy eq x ys

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  This (xs ++ ys) \\ xs == ys.
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\)		        =  foldl (flip delete)

-- Alternate name for \\
deleteFirsts		:: (Eq a) => [a] -> [a] -> [a]
deleteFirsts 		= (\\)

deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

-- elem, notElem, lookup, maximumBy and minimumBy are in PreludeList
elemBy, notElemBy       :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []		=  False
elemBy eq x (y:ys)	=  x `eq` y || elemBy eq x ys

notElemBy eq x xs       =  not (elemBy eq x xs)

lookupBy                :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq key []      =  Nothing
lookupBy eq key ((x,y):xys)
    | key `eq` x	=  Just y
    | otherwise		=  lookupBy eq key xys

maximumBy		:: (a -> a -> a) -> [a] -> a
maximumBy max []	=  error "List.maximumBy: empty list"
maximumBy max xs	=  foldl1 max xs

minimumBy		:: (a -> a -> a) -> [a] -> a
minimumBy min []	=  error "List.minimumBy: empty list"
minimumBy min xs	=  foldl1 min xs

-- nub (meaning "essence") remove duplicate elements from its list argument.
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)

nubBy			:: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)

-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i,e,,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p xs		=  foldr select ([],[]) xs
			   where select x (ts,fs) | p x       = (x:ts,fs)
                                                  | otherwise = (ts, x:fs)

-- sums and products give a list of running sums or products from
-- a list of numbers.  e.g., sums [1,2,3] == [0,1,3,6]
sums, products		:: (Num a) => [a] -> [a]
sums 			=  scanl (+) 0 
products 		=  scanl (*) 1 

transpose		:: [[a]] -> [[a]]
transpose		=  foldr
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     []

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

genericLength           :: (Num i) => [b] -> i
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l

genericDrop		:: (Integral i) => i -> [a] -> [a]
genericDrop 0 xs        =  xs
genericDrop _ []        =  []
genericDrop n (_:xs) | n > 0  =  genericDrop (n-1) xs
genericDrop _ _		=  error "List.genericDrop: negative argument"

genericTake		:: (Integral i) => i -> [a] -> [a]
genericTake 0 _         =  []
genericTake _ []        =  []
genericTake n (x:xs) | n > 0  =  x : genericTake (n-1) xs
genericTake _  _        =  error "List.genericTake: negative argument"

genericSplitAt          :: (Integral i) => i -> [b] -> ([b],[b])
genericSplitAt 0 xs     =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) | n > 0  =  (x:xs',xs'') where
                               (xs',xs'') = genericSplitAt (n-1) xs
genericSplitAt _ _      =  error "List.genericSplitAt: negative argument"

genericReplicate	:: (Integral i) => i -> a -> [a]
genericReplicate n x	=  genericTake n (repeat x)

-- l !! (elemIndex l x) == x  if x `elem` l
elemIndex  		:: Eq a => [a] -> a -> Int
elemIndex 		=  elemIndexBy (==)

elemIndexBy 		:: (a -> a -> Bool) -> [a] -> a -> Int
elemIndexBy eq [] x 	 = error "List.elemIndexBy: empty list"
elemIndexBy eq (x:xs) x' = if x `eq` x' then 0 else 1 + elemIndexBy eq xs x'

-- group splits its list argument into a list of lists of equal, adjacent
-- elements.  e.g.,
-- group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
group                   :: (Eq a) => [a] -> [[a]]
group                   =  groupBy (==)

groupBy 		:: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq []		=  []
groupBy eq (x:xs)	=  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs
                           

mapAccumL 		:: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f s []     	=  (s, [])
mapAccumL f s (x:xs) 	=  (s'',y:ys)
		           where (s', y ) = f s x
			         (s'',ys) = mapAccumL f s' xs

mapAccumR 		:: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f s []     	=  (s, [])
mapAccumR f s (x:xs)	=  (s'', y:ys)
		           where (s'',y ) = f s' x
			         (s', ys) = mapAccumR f s xs

-- intersperse sep inserts sep between the elements of its list argument.
-- e.g. intersperse ',' "abcde" == "a,b,c,d,e"
intersperse             :: a -> [a] -> [a]
intersperse sep []      =  []
intersperse sep [x]     =  [x]
intersperse sep (x:xs)  =  x : sep : intersperse sep xs

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

-- subsequences xs returns the list of all subsequences of xs.
-- e.g., subsequences "abc" == ["","c","b","bc","a","ac","ab","abc"]
subsequences 		:: [a] -> [[a]]
subsequences []		=  [[]]
subsequences (x:xs) 	=  subsequences xs ++ map (x:) (subsequences xs)

-- permutations xs returns the list of all permutations of xs.
-- e.g., permutations "abc" == ["abc","bac","bca","acb","cab","cba"]
permutations            :: [a] -> [[a]]
permutations []         =  [[]]
permutations (x:xs)     =  [zs | ys <- permutations xs, zs <- interleave x ys ]
  where interleave          :: a -> [a] -> [[a]]
        interleave x []     =  [[x]]
        interleave x (y:ys) =  [x:y:ys] ++ map (y:) (interleave x ys)

union 			:: (Eq a) => [a] -> [a] -> [a]
union xs ys 		=  xs ++ (ys \\ xs)

intersect 		:: (Eq a) => [a] -> [a] -> [a]
intersect xs ys 	=  [x | x <- xs, x `elem` ys]


