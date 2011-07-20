module QSort where


qsort [] = [] 
qsort (a:as) = (qsort left) ++ [a] ++ (qsort right)
 where (left,right) = (filter (<=a) as, filter (>a) as)

run = qsort [8, 4, 0, 3, 1, 23, 11, 18]

-- > run
-- [0,1,3,4,8,11,18,23]