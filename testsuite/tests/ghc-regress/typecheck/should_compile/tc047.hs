module ShouldSucceed where

type OL a =  [a]

-- produces the interface:
-- data OL a = MkOL [a] deriving ()
-- ranOAL :: (OL (a, a)) -> [a]
-- this interface was produced by BOTH hbc and nhc

-- the following bogus type sig. was accepted by BOTH hbc and nhc
f x = ranOAL where -- ranOAL :: OL (a,v) -> [a]
--ranOAL :: OL (a,v) -> [v], the right sig.
		   ranOAL ( xs) = mp sd xs


mp f [] = []
mp f (x:xs) = (f x) : mp f xs

sd (f,s) = s




