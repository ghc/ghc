#if __HASKELL1__ < 3
module ListUtil(assoc, concatMap, unfoldr, mapAccuml, union, intersection, chopList, assocDef, lookup, Maybe..,
                rept, tails, groupEq, group, readListLazily, nubEq, elemEq) where
import {-flummox mkdependHS-}
	Maybe
#else
module ListUtil(assoc, concatMap, unfoldr, mapAccuml, union, intersection, chopList, assocDef, lookup, -- Maybe..,
                rept, tails, groupEq, group, readListLazily, nubEq, elemEq) where
--import Maybe
#endif

-- Lookup an item in an association list.  Apply a function to it if it is found, otherwise return a default value.
assoc :: (Eq c) => (a -> b) -> b -> [(c, a)] -> c -> b
assoc f d [] x                       = d
assoc f d ((x',y):xys) x | x' == x   = f y
                         | otherwise = assoc f d xys x

-- Map and concatename results.
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []	   = []
concatMap f (x:xs) =
	case f x of
	[] -> concatMap f xs
	ys -> ys ++ concatMap f xs

-- Repeatedly extract (and transform) values until a predicate hold.  Return the list of values.
unfoldr :: (a -> (b, a)) -> (a -> Bool) -> a -> [b]
unfoldr f p x | p x       = []
	      | otherwise = y:unfoldr f p x'
			      where (y, x') = f x

-- Map, but plumb a state through the map operation.
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f s []     = (s, [])
mapAccuml f s (x:xs) = (s'', y:ys)
		       where (s',  y)  = f s x
			     (s'', ys) = mapAccuml f s' xs

-- Union of sets as lists.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ (ys \\ xs)

-- Intersection of sets as lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x<-xs, x `elem` ys]

--- Functions derived from those above

chopList :: ([a] -> (b, [a])) -> [a] -> [b]
chopList f l = unfoldr f null l

assocDef :: (Eq a) => [(a, b)] -> b -> a -> b
--assocDef l d x = assoc id d l x
assocDef [] d _ = d
assocDef ((x,y):xys) d x' = if x == x' then y else assocDef xys d x'

lookup :: (Eq a) => [(a, b)] -> a -> Maybe b
--lookup l x = assoc Just Nothing l x
lookup [] _ = Nothing
lookup ((x,y):xys) x' = if x == x' then Just y else lookup xys x'

-- Repeat an element n times
rept :: (Integral a) => a -> b -> [b]
rept n x = irept (fromIntegral n) x
	where irept :: Int -> a -> [a]
	      irept n x = if n <= 0 then [] else x : irept (n-1) x

-- Take all the tails
tails :: [a] -> [[a]]
tails []         = []
tails xxs@(_:xs) = xxs : tails xs

-- group list elements according to an equality predicate
groupEq :: (a->a->Bool) -> [a] -> [[a]]
groupEq eq xs = chopList f xs
		where f xs@(x:_) = span (eq x) xs

group :: (Eq a) => [a] -> [[a]]
group xs = groupEq (==) xs

-- Read a list lazily (in contrast with reads which requires
-- to see the ']' before returning the list.
readListLazily :: (Text a) => String -> [a]
readListLazily cs = 
    case lex cs of
      [("[",cs)] -> readl' cs
      _          -> error "No leading '['"
    where readl' cs  =
                case reads cs of
                  [(x,cs)]  -> x : readl cs
                  []        -> error "No parse for list element"
                  _         -> error "Ambigous parse for list element"
          readl cs =
                case lex cs of
                  [("]",_)]  -> []
                  [(",",cs)] -> readl' cs
                  _          -> error "No ',' or ']'"

nubEq :: (a->a->Bool) -> [a] -> [a]
nubEq eq l = nub' l []
	where nub' [] _	    = []
	      nub' (x:xs) l = if elemEq eq x l then nub' xs l else x : nub' xs (x:l)

elemEq :: (a->a->Bool) -> a -> [a] -> Bool
elemEq eq _ []	   = False
elemEq eq x (y:ys) = eq x y || elemEq eq x ys

mapFst f xys = [(f x, y) | (x, y) <- xys]
mapSnd f xys = [(x, f y) | (x, y) <- xys]
