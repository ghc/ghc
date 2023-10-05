qsort :: [Int] -> [Int]
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main = print $ qsort [4, 1, 7, 10, 3]
