module Auxil where

import Key

data Key = K String Char Char Int {- String, end letters, length of string -}
data HashSet = H (Maybe Int) (Maybe Int) [Int]
type HashFun = [(Char,Int)]  {- Association list of Character to values -}
--1.3:data Maybe a = Nothing | Just a deriving Text

ends :: Key -> String
ends (K _ a z _) = [a,z]

morefreq :: Key -> Key -> Bool
morefreq (K _ a x _) (K _ b y _) = freq a + freq x > freq b + freq y

freq :: Char -> Int
freq c = assoc c freqtab

assoc :: (Eq a) => a -> [(a,b)] -> b
assoc x ((y,z):yzs) = if x == y then z else assoc x yzs

assocm :: (Eq a) => a -> [(a,b)] -> Maybe b
assocm x [] = Nothing
assocm x ((y,z):yzs) = if x == y then Just z else assocm x yzs

freqtab :: [(Char, Int)]
freqtab = histo (concat (map ends attribkeys))

histo :: (Eq a) => [a] -> [(a,Int)]
histo = foldr histins []
        where
        histins x [] = [(x,1)]
        histins x (yn@(y,n):yns) = if x==y then (y,n+1):yns
                                   else yn:histins x yns

maxval :: Int
maxval = length (freqtab)

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (\x -> member x ys) xs
 
--partain: in the prelude
--all :: (a->Bool) -> [a] -> Bool
--all p = foldr (\x -> \b ->(p x && b)) True
 
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (member y xs)]
 
attribkeys :: [Key]
attribkeys = map (\k->(K k (head k) (last k) (length k))) keys
 
hinsert :: Int -> HashSet -> Maybe HashSet
hinsert h (H lo hi hs) =
    if member h hs || 1 + hi'- lo' > numberofkeys then Nothing
    else Just (H (Just lo') (Just hi') (h:hs))
    where
    lo' = minm lo h
    hi' = maxm hi h
 
minm, maxm :: Maybe Int -> Int -> Int
minm Nothing y = y
minm (Just x) y = min x y
maxm Nothing y = y
maxm (Just x) y = max x y
 
member :: (Eq a) => a -> [a] -> Bool
member _ [] = False
member x (y:ys) = x == y || member x ys
 
hash :: HashFun -> Key -> Int
hash cvs (K _ a z n) = n + assoc a cvs + assoc z cvs
 
numberofkeys :: Int
numberofkeys = length keys
 

partition' :: (a->Bool) -> [a] -> ([a],[a])
partition' p = foldr select ([],[])
              where select x (ts,fs) | p x       = (x:ts,fs)
                                     | otherwise = (ts,x:fs)

freqsorted :: [Key] -> [Key]
freqsorted =
	\x->x
    {-foldr freqins []
    where
    freqins x [] = [x]
    freqins x (y:ys) = if morefreq x y then x:y:ys else y:freqins x ys-}
 
blocked :: [Key] -> [Key]
blocked = blocked' []
blocked' ds [] = []
blocked' ds (k : ks) = k : det ++ blocked' ds' rest
                     where
                     (det,rest) = partition' (\x->subset (ends x) ds') ks
                     ds' = union ds (ends k)

