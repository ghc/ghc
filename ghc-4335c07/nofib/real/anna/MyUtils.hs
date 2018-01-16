
-- ==========================================================--
-- === Various useful bits & pieces                       ===--
-- ===                                         MyUtils.hs ===--
-- ==========================================================--

module MyUtils where
import BaseDefs

infixl 9 ##

-- ==========================================================--
--
myFail msg
   = error ("\n" ++ msg ++ "\n")

panic msg
   = error ("\nPanic! (the `impossible' happened):\n" ++ msg ++ "\n")


-- ==========================================================--
--
mySubtract :: Int -> Int -> Int

mySubtract x y = y - x


-- ==========================================================--
--
myZipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith2 f []     []     = []
myZipWith2 f (a:as) (b:bs) = f a b : myZipWith2 f as bs
myZipWith2 _ _      _      = panic "myZipWith2: unequal lists"

myZip2 = myZipWith2 (\a b -> (a, b))


-- ==========================================================--
--
myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

myZipWith3 f [] [] [] = []
myZipWith3 f (a:as) (b:bs) (c:cs) = f a b c : myZipWith3 f as bs cs
myZipWith3 _ _      _      _      = panic "myZipWith3: unequal lists"

myZip3 = myZipWith3 (\a b c -> (a, b, c))


-- ==========================================================--
--
myZipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]

myZipWith4 f [] [] [] [] = []
myZipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : myZipWith4 f as bs cs ds
myZipWith4 _ _      _      _      _      = panic "myZipWith4: unequal lists"

myZip4 = myZipWith4 (\a b c d -> (a, b, c, d))


-- ==========================================================--
--
myZipWith5 :: (a -> b -> c -> d -> e -> f) -> 
              [a] -> [b] -> [c] -> [d] -> [e] -> [f]

myZipWith5 f [] [] [] [] [] = []
myZipWith5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es)
   = f a b c d e : myZipWith5 f as bs cs ds es
myZipWith5 _ _      _      _      _      _
   = panic "myZipWith5: unequal lists"

myZip5 = myZipWith5 (\a b c d e -> (a, b, c, d, e))


-- ==========================================================--
--
myAndWith2 :: (a -> b -> Bool) -> [a] -> [b] -> Bool

myAndWith2 f []     []
   = True

myAndWith2 f (a:as) (b:bs)
   = if     f a b
     then   myAndWith2 f as bs
     else   False

myAndWith2 _ _      _
   = panic "myAndWith2: unequal lists"


-- ==========================================================--
--
myAny, myAll :: (a -> Bool) -> [a] -> Bool

myAny p []       = False
myAny p (x:xs)   = if p x then True else myAny p xs

myAll p []       = True
myAll p (x:xs)   = if p x then myAll p xs else False


-- ==========================================================--
--
myAnd, myOr :: [Bool] -> Bool

myAnd []        = True
myAnd (x:xs)    = if x then myAnd xs else False

myOr  []        = False
myOr  (x:xs)    = if x then True else myOr xs


-- ==========================================================--
--
myListVariants :: [a] -> [[a]] -> [[a]]

myListVariants [] [] = []

myListVariants (x:xs) (rs:rss)
   = map ((flip (:)) xs) rs ++ map (x:) (myListVariants xs rss)

myListVariants _ _ = panic "myListVariants: unequal lists"


-- ==========================================================--
--
myCartesianProduct :: [[a]] -> [[a]]

myCartesianProduct [] 
   = [[]]

myCartesianProduct (xs:xss)
   = let g as bs = map (:bs) as
     in
         concat (map (g xs) (myCartesianProduct xss))


-- ==========================================================--
--
mySeq :: (Eq a) => a -> b -> b

mySeq x y | x == x = y


-- ==========================================================--
--
myIntsFromTo :: Int -> Int -> [Int]

myIntsFromTo n m
   = if     n > m
     then   []
     else   n : myIntsFromTo (n + (1 :: Int)) m


-- ==========================================================--
--
myIntsFrom :: Int -> [Int]

myIntsFrom n = n : myIntsFrom (n + (1 :: Int))


-- ==========================================================--
--
(##) :: [b] -> Int -> b

[] ## n
   = panic "(##) (1)"
(x:xs) ## n
   = if n == (0 :: Int) then x else xs ## (n - (1 :: Int))


-- ==========================================================--
-- === end                                     MyUtils.hs ===--
-- ==========================================================--
