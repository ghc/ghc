module Main(main) where

scoreWeek :: [Int] -> [[Int]]
scoreWeek xs =
  take 168
  $ scanr (:) []
  -- $ take (3*168)
  $ cycle xs

main = print $ length $ scoreWeek [1,2..168]

