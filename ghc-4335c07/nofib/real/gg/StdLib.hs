 module StdLib where
 pair :: a -> b -> (a,b)
 pair a b = (a,b)
 fstcons :: a -> ([a],b) -> ([a],b)
 fstcons a (as,b) = (a:as,b)
 sndcons :: b -> (a,[b]) -> (a,[b])
 sndcons b (a,bs) = (a,b:bs)

 map2 f [] _ = []
 map2 f _ [] = []
 map2 f (a:l) (b:k) = f a b:map2 f l k

 mapcat :: (a->[b]) -> [a] -> [b]
 mapcat f [] = []
 mapcat f (a:l) = f a ++ mapcat f l
 sort :: Ord a => [a] -> [a]
 sort [] = []
 sort (a:l) = (sort low) ++ [a] ++ (sort high)
 	where 	(low,high) = group a l
 group :: Ord a => a -> [a] -> ([a],[a])
 group _ [] = ([],[])
 group i (a:l) = f (group i l) 
		where f (low,high) | a<i = (a:low,high)
		 		   | otherwise = (low,a:high)
 insert :: Ord a => a -> [a] -> [a]
 insert a [] = [a]
 insert a as@(x:xs) | x>a = a:as
 		     | otherwise = x:insert a xs
 replace :: Eq a => a -> [a] -> [a]
 replace a [] = []
 replace a (x:xs) | a == x = a:xs 
 		   | otherwise = x:replace a xs
 remove :: Eq a => a -> [a] -> [a]
 remove a [] = []
 remove a (x:xs) | a==x = xs
 		  | otherwise = x:remove a xs
 collect :: Ord a => (b->a) -> [a] -> [b] -> [b]
 collect _ [] _ = []
 collect _ _ [] = []
 collect p as@(a:l) bs@(b:k) | a==p b = b:collect p l k
			      | p b > a = collect p l bs
			      | otherwise = collect p as k
 span' :: (a->Bool) -> [a] -> ([a],[a])
 span' p [] = ([],[])
 span' p (x:xs') | p x = fixLeak x (span' p xs') where fixLeak x (xs,ys) = (x:xs,ys)
 span' _ xs = ([],xs)
 lines' :: [Char] -> [[Char]]
 lines' "" = []
 lines' s = plumb (span' ((/=) '\n') s)
        where   plumb (l,s') = l:if null s' then [] else lines' (tail s')
 strToInt :: String -> Int
 strToInt x = strToInt' (length x-1) x
       where   strToInt' _ [] = 0
               strToInt' x (a:l) = (charToInt a)*(10^x) + (strToInt' (x-1) l)
 charToInt :: Char -> Int
 charToInt x = (fromEnum x - fromEnum '0')
