{-# LANGUAGE RankNTypes #-}

module ShouldSucceed where

data SeqView t a              =  Null
                              |  Cons a (t a)

class PriorityQueue q where
    empty			:: (Ord a) => q a
    single			:: (Ord a) => a -> q a
    insert			:: (Ord a) => a -> q a -> q a
    meld			:: (Ord a) => q a -> q a -> q a
    splitMin			:: (Ord a) => q a -> SeqView q a
    insert a q		=  single a `meld` q

toOrderedList q		=  case splitMin q of
   Null			-> []
   Cons a q		-> a : toOrderedList q

insertMany x q		=  foldr insert q x
pqSort q x		=  toOrderedList (insertMany x q)

check			:: forall q. (PriorityQueue q) => (forall a. Ord a => q a) -> IO ()
check empty		=  do
    putStr "*** sorting\n"
    out (pqSort empty [1 .. 99])
    out (pqSort empty [1.0, 1.1 ..99.9])

out				:: (Eq a, Num a) => [a] -> IO ()
out x | sum x == 0		=  putStr "ok\n"
      | otherwise		=  putStr "ok\n"

