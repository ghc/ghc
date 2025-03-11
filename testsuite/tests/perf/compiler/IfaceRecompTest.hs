{-# LANGUAGE NoImplicitPrelude #-}
module IfaceRecompTest where

import Prelude (Eq(..), Bool(..), Maybe(..), Int, Ord(..), Either(..), String, Char
               , elem, not, filter, (++), otherwise, reverse, Num(..), (&&), (||), seq, (.)
               , dropWhile, break, Enum(..), map, length, replicate, concat, fromIntegral
               , RealFrac(..), mod, sqrt, all, div )

-- | Adds two integers
add :: Int -> Int -> Int
add x y = x + y

-- | Subtracts second integer from first
subtract :: Int -> Int -> Int
subtract x y = x - y

-- | Multiplies two integers
multiply :: Int -> Int -> Int
multiply x y = x * y

-- | Divides first integer by second
divide :: Int -> Int -> Int
divide x y = x `div` y

-- | Calculates square of an integer
square :: Int -> Int
square x = x * x

-- | Calculates cube of an integer
cube :: Int -> Int
cube x = x * x * x

-- | Calculates factorial of a non-negative integer
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | Checks if a number is even
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- | Checks if a number is odd
isOdd :: Int -> Bool
isOdd = not . isEven

-- | Calculates the greatest common divisor of two integers
gcd :: Int -> Int -> Int
gcd a 0 = abs a
gcd a b = gcd b (a `mod` b)

-- | Calculates the least common multiple of two integers
lcm :: Int -> Int -> Int
lcm a b = abs (a * b) `div` gcd a b

-- | Checks if a number is prime
isPrime :: Int -> Bool
isPrime n | n <= 1 = False
isPrime 2 = True
isPrime n = all (\x -> n `mod` x /= 0) [2..isqrt n]
  where isqrt = floor . sqrt . fromIntegral

-- | Concatenates two strings
concatStrings :: String -> String -> String
concatStrings = (++)

-- | Repeats a string n times
repeatString :: String -> Int -> String
repeatString str n = concat (replicate n str)

-- | Reverses a string
reverseString :: String -> String
reverseString = reverse

-- | Capitalizes the first letter of a string
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs
  where toUpper c | 'a' <= c && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
                 | otherwise = c

-- | Calculates the length of a string
stringLength :: String -> Int
stringLength = length

-- | Converts a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper
  where toUpper c | 'a' <= c && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
                 | otherwise = c

-- | Converts a string to lowercase
toLowercase :: String -> String
toLowercase = map toLower
  where toLower c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
                 | otherwise = c

-- | Checks if a string contains a substring
contains :: String -> String -> Bool
contains str substr = isJust (findSubstring substr str)
  where
    findSubstring [] _ = Just []
    findSubstring _ [] = Nothing
    findSubstring pat@(p:ps) (s:ss)
      | p == s && isPrefixOf ps ss = Just pat
      | otherwise = findSubstring pat ss
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (p:ps) (s:ss) = p == s && isPrefixOf ps ss
    isJust Nothing = False
    isJust (Just _) = True

-- | Splits a string by a delimiter
splitString :: Char -> String -> [String]
splitString delim str = case break (== delim) str of
  (a, []) -> [a]
  (a, _:rest) -> a : splitString delim rest

-- | Joins a list of strings with a delimiter
joinStrings :: String -> [String] -> String
joinStrings delim = intercalate delim
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Trims whitespace from both ends of a string
trim :: String -> String
trim = trimEnd . trimStart
  where
    trimStart = dropWhile isSpace
    trimEnd = reverse . trimStart . reverse
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Returns the first n elements of a list
take :: Int -> [a] -> [a]
take _ [] = []
take n _ | n <= 0 = []
take n (x:xs) = x : take (n-1) xs

-- | Drops the first n elements from a list
drop :: Int -> [a] -> [a]
drop _ [] = []
drop n xs | n <= 0 = xs
drop n (_:xs) = drop (n-1) xs

-- | Splits a list at the nth position
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- | Maps a function over a list
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-- | Filters a list based on a predicate
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
  | p x = x : filterList p xs
  | otherwise = filterList p xs

-- | Folds a list from the left
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = let acc' = f acc x in acc' `seq` foldl' f acc' xs

-- | Folds a list from the right
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- | Removes duplicate elements from a list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- | Sorts a list
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]

-- | Merges two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- | Zips two lists together
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (a:as) (b:bs) = (a, b) : zipLists as bs

-- | Unzips a list of pairs
unzipLists :: [(a, b)] -> ([a], [b])
unzipLists [] = ([], [])
unzipLists ((a, b):ps) = let (as, bs) = unzipLists ps in (a:as, b:bs)

-- | Creates a list with n copies of a value
replicate' :: Int -> a -> [a]
replicate' n x | n <= 0 = []
replicate' n x = x : replicate' (n-1) x

-- | Returns Just the first element satisfying a predicate, or Nothing
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x = Just x
  | otherwise = find p xs

-- | Returns all elements satisfying a predicate
findAll :: (a -> Bool) -> [a] -> [a]
findAll = filterList

-- | Checks if any element satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

-- | Checks if all elements satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

-- | Safe version of head
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- | Safe version of tail
tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_:xs) = Just xs

-- | Safe version of last
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_:xs) = lastMaybe xs

-- | Safe version of init
initMaybe :: [a] -> Maybe [a]
initMaybe [] = Nothing
initMaybe [_] = Just []
initMaybe (x:xs) = case initMaybe xs of
                     Nothing -> Nothing
                     Just ys -> Just (x:ys)

-- | Safe list indexing
at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:_) 0 = Just x
at (_:xs) n | n > 0 = at xs (n-1)
            | otherwise = Nothing

-- | Maps a function over a Maybe
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- | Returns the Just value or a default
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

-- | Converts a Maybe to a list
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- | Maps a function over the Left value of an Either
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

-- | Maps a function over the Right value of an Either
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x) = Left x
mapRight f (Right y) = Right (f y)

-- | Converts an Either to a Maybe, discarding the Left value
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right y) = Just y

-- | A simple pair type
data Pair a b = Pair a b

-- | Creates a pair
makePair :: a -> b -> Pair a b
makePair = Pair

-- | Gets the first element of a pair
fst' :: Pair a b -> a
fst' (Pair a _) = a

-- | Gets the second element of a pair
snd' :: Pair a b -> b
snd' (Pair _ b) = b

-- | Swaps the elements of a pair
swap :: Pair a b -> Pair b a
swap (Pair a b) = Pair b a

-- | A simple triple type
data Triple a b c = Triple a b c

-- | Creates a triple
makeTriple :: a -> b -> c -> Triple a b c
makeTriple = Triple

-- | Gets the first element of a triple
fst3 :: Triple a b c -> a
fst3 (Triple a _ _) = a

-- | Gets the second element of a triple
snd3 :: Triple a b c -> b
snd3 (Triple _ b _) = b

-- | Gets the third element of a triple
thd3 :: Triple a b c -> c
thd3 (Triple _ _ c) = c

-- | A simple tree type
data Tree a = Leaf | Node a (Tree a) (Tree a)

-- | Creates a leaf node
leaf :: Tree a
leaf = Leaf

-- | Creates a tree node
node :: a -> Tree a -> Tree a -> Tree a
node = Node

-- | Checks if a tree is empty
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- | Returns the value at the root of a tree, if any
rootValue :: Tree a -> Maybe a
rootValue Leaf = Nothing
rootValue (Node x _ _) = Just x

-- | Returns the left subtree
leftSubtree :: Tree a -> Tree a
leftSubtree Leaf = Leaf
leftSubtree (Node _ left _) = left

-- | Returns the right subtree
rightSubtree :: Tree a -> Tree a
rightSubtree Leaf = Leaf
rightSubtree (Node _ _ right) = right

-- | Inserts a value into a binary search tree
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x Leaf = Node x Leaf Leaf
insertBST x (Node y left right)
  | x < y     = Node y (insertBST x left) right
  | x > y     = Node y left (insertBST x right)
  | otherwise = Node x left right

-- | Searches for a value in a binary search tree
searchBST :: Ord a => a -> Tree a -> Bool
searchBST _ Leaf = False
searchBST x (Node y left right)
  | x == y    = True
  | x < y     = searchBST x left
  | otherwise = searchBST x right

-- | Performs an in-order traversal of a tree
inOrderTraversal :: Tree a -> [a]
inOrderTraversal Leaf = []
inOrderTraversal (Node x left right) = inOrderTraversal left ++ [x] ++ inOrderTraversal right

-- | Performs a pre-order traversal of a tree
preOrderTraversal :: Tree a -> [a]
preOrderTraversal Leaf = []
preOrderTraversal (Node x left right) = [x] ++ preOrderTraversal left ++ preOrderTraversal right

-- | Performs a post-order traversal of a tree
postOrderTraversal :: Tree a -> [a]
postOrderTraversal Leaf = []
postOrderTraversal (Node x left right) = postOrderTraversal left ++ postOrderTraversal right ++ [x]

-- | Calculates the height of a tree
treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- | Calculates the size of a tree (number of nodes)
treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- | Maps a function over a tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

-- | Folds a tree from the left
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node x left right) =
  f x (foldTree f acc left) (foldTree f acc right)

-- | A simple queue type
data Queue a = Queue [a] [a]

-- | Creates an empty queue
emptyQueue :: Queue a
emptyQueue = Queue [] []

-- | Checks if a queue is empty
isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue [] []) = True
isEmptyQueue _ = False

-- | Adds an element to a queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue ins outs) = Queue (x:ins) outs

-- | Removes an element from a queue
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue ins []) = dequeue (Queue [] (reverse ins))
dequeue (Queue ins (x:xs)) = Just (x, Queue ins xs)

-- | A simple stack type
data Stack a = Stack [a]

-- | Creates an empty stack
emptyStack :: Stack a
emptyStack = Stack []

-- | Checks if a stack is empty
isEmptyStack :: Stack a -> Bool
isEmptyStack (Stack []) = True
isEmptyStack _ = False

-- | Pushes an element onto a stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | Pops an element from a stack
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- | Peeks at the top element of a stack without removing it
peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

-- | A simple set type
newtype Set a = Set [a]

-- | Creates an empty set
emptySet :: Set a
emptySet = Set []

-- | Checks if a set is empty
isEmptySet :: Set a -> Bool
isEmptySet (Set []) = True
isEmptySet _ = False

-- | Adds an element to a set
addToSet :: Eq a => a -> Set a -> Set a
addToSet x (Set xs)
  | x `elem` xs = Set xs
  | otherwise   = Set (x:xs)

-- | Removes an element from a set
removeFromSet :: Eq a => a -> Set a -> Set a
removeFromSet x (Set xs) = Set (filter (/= x) xs)

-- | Checks if an element is in a set
inSet :: Eq a => a -> Set a -> Bool
inSet x (Set xs) = x `elem` xs

-- | Converts a set to a list
setToList :: Set a -> [a]
setToList (Set xs) = xs

-- | Calculates the union of two sets
unionSets :: Eq a => Set a -> Set a -> Set a
unionSets (Set xs) (Set ys) = Set (nub (xs ++ ys))
  where
    nub [] = []
    nub (z:zs) = z : nub (filter (/= z) zs)

-- | Calculates the intersection of two sets
intersectSets :: Eq a => Set a -> Set a -> Set a
intersectSets (Set xs) (Set ys) = Set [x | x <- xs, x `elem` ys]

-- | Calculates the difference of two sets
diffSets :: Eq a => Set a -> Set a -> Set a
diffSets (Set xs) (Set ys) = Set [x | x <- xs, not (x `elem` ys)]
