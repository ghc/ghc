-- test for derived Ord instances

module TestOrd where

data T = C1 | C2 deriving (Eq, Ord)

cmps :: [T -> T -> Bool]
cmps = [ (<), (<=), (==), (/=), (>=), (>) ]

-- kind of a reversed zipWith...
rzipWith :: [a -> b -> c] -> a -> b -> [c]
rzipWith fs a b = [ f a b | f <- fs ]

--!!! Testing derived Ord and Eq instances for enumeration type
test1 = rzipWith cmps C1 C1 -- should be [F,T,T,F,T,F]
test2 = rzipWith cmps C1 C2 -- should be [T,T,F,T,F,F]
test3 = rzipWith cmps C2 C1 -- should be [F,F,F,T,T,T]

