import System.Environment

qsort :: [Int] -> [Int]
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main :: IO()
main = do
  args <- getArgs
  print $ qsort $ map read $ args
