{- 
From: Marc van Dongen <dongen@cs.ucc.ie>
Date: Wed, 9 Apr 1997 14:06:39 +0100 (BST)

I just wanted to report that the erroneous and tiny
program added below can not be compiled within 6MB of
heap (Admitted it can be compiled with a bigger heap).
It was part of a bigger program that could not be
compiled within 20MB of heap.

[GHC 2.03 and earlier.]  Turned out to be a bug in the
error recovery mechanism.

-}

module ShouldFail where

too_much :: [Int] -> [(Int,Int)] -> [(Int,[Int])] -> Bool
too_much ds ((k,m):q1) s0
  = case (list1,list2) of
      []  -> error "foo" -- too_much ds q2m  s2m
  where list1 = ds
	list2 = ds
	{-
	list1 = [k' | k' <- ds, k == k']
        list2 = [k' | k' <- ds, m == k']
        s1   = aas s0 k
        raM  = []
        raKM = listUnion (\a b -> a) [] []
        s1k  = s1
        q1k  = raM
        s2k  = s1
        q2k  = raM
        s2m  = s1
        q2m  = raM
        s2km = foldr (flip aas) s1 raKM
        q2km = raKM
	-}

listUnion :: (v -> v -> Bool) -> [v] -> [v] -> [v]
listUnion _  _ _
  = []

aas :: (a,b) -> a -> (a,b)
aas s _
  = s


