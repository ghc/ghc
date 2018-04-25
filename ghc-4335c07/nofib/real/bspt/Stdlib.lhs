> module Stdlib


	Collection of Standard functions



>	(	mapcat,map2,mappair,seQuence,
> 		middle,mkset,between,pair,--UNUSED: pairUp,curry,
> 		{-const,-}const3,splitAt_YORK,numval,toNum,all_YORK)

> where

	mapcat - map a function over a list and concatenates the results

> mapcat :: (a->[b]) -> [a] -> [b]
> mapcat f x = foldr (++) [] (map f x) 


	map2 - maps a binary function over two lists to produce a result

> map2 :: (a->b->c) -> [a] -> [b] -> [c]
> map2 _ [] l = []
> map2 _ l [] = []
> map2 f (x:xs) (y:ys) = (f x y):(map2 f xs ys)

	mkset - returns a list with duplicates of elements removed.

> mkset [] = []
> mkset (a:l) = a:mkset ((filter ((/=) a)) l) -- "remove" no longer in prelude

	mappair - map a function over both elements of a pair

> mappair :: (a->b) -> (a,a) -> (b,b)
> mappair f (a,b) = (f a,f b)


	middle - determine the halfway point two integers

> middle :: Int-> Int-> Int
> middle x y = div (x+y) 2


	between - predicate testing value for location between
		base and base + offset
	
> between :: (Ord a,Num a) => a -> a -> a -> Bool
> between base offset value = (base <= value) && (value <= (base+offset))


	pair - pairing function

> pair :: a -> b -> (a,b)
> pair x y = (x,y)


	const - returns the first of the two arguments

> {-
> const :: a -> b -> a
> const x y = x
> -}

	const - returns the first of the four arguments

> const3 :: a -> b -> c -> d -> a
> const3 w x y z = w


	splitAt_YORK - splits a list of values at a value given into a pair
		of values - those strictly before and those strictly after

> splitAt_YORK ::(Eq a) => a -> [a] -> ([a],[a])
> splitAt_YORK ch [] = ([],[])
> splitAt_YORK ch (a:l) | a==ch = ([], l)
> splitAt_YORK ch (a:l) | otherwise = (a:x,y)
>                         where (x,y) = splitAt_YORK ch l


	numval - converts a decimal string to an integer (Int)

> numval :: String -> Int
> numval x = numval' (length x-1) x
>                 where numval' _ [] = 0
>                       numval' x (a:l) = (toNum a)*(10^x) + (numval' (x-1) l)


	toNum - converts a numeric character to the corresponding
		int.

> toNum :: Char -> Int
> toNum x = (fromEnum x - fromEnum '0')

 
        seQuence: Concatenates a list of strings.

> seQuence :: [String] -> String
> seQuence = concat

	all_YORK : rename the badly named prelude function
		and.

> all_YORK :: [Bool] -> Bool
> all_YORK = and
