{-
   This module implements a sort function using a variation on
   quicksort.  It is stable, uses no concatenation and compares
   only with <=.
  
   sortLe sorts with a given predicate
   sort   uses the <= method
  
   Author: Lennart Augustsson
-}

module QSort(sortLe, sort) where
sortLe :: (a -> a -> Bool) -> [a] -> [a]
sortLe le l = qsort le   l []

sort :: (Ord a) => [a] -> [a]
sort      l = qsort (<=) l []

-- qsort is stable and does not concatenate.
qsort le []     r = r
qsort le [x]    r = x:r
qsort le (x:xs) r = qpart le x xs [] [] r

-- qpart partitions and sorts the sublists
qpart le x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort le rlt (x:rqsort le rge r)
qpart le x (y:ys) rlt rge r =
    if le x y then
	qpart le x ys rlt (y:rge) r
    else
	qpart le x ys (y:rlt) rge r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort le []     r = r
rqsort le [x]    r = x:r
rqsort le (x:xs) r = rqpart le x xs [] [] r

rqpart le x [] rle rgt r =
    qsort le rle (x:qsort le rgt r)
rqpart le x (y:ys) rle rgt r =
    if le y x then
	rqpart le x ys (y:rle) rgt r
    else
	rqpart le x ys rle (y:rgt) r

